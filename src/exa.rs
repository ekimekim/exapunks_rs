

// XXX: We could pack this tighter (into 2 bytes) if we wanted,
// since we don't actually need the full range of either enum's data.
/// Value is the contents of files, general purpose registers, etc
/// and may be a number from -9999 to 9999 or a symbol.
#[derive(Debug,Clone,Copy,PartialEq,Eq)]
enum Value {
	// We use i16 for number here but only need -9999 to 9999
	Number(i16),
	// Rather than storing the full symbol name here, we just store a "symbol id"
	// which is the index of the symbol in a sorted list of all extant symbols
	// (pre-defined before the start of emulation).
	// Since the ids are ordered, this allows us to test for equality and ordering
	// which are the only operations supported anyway.
	Symbol(u16),
}

impl Value {
	fn operate(self, op: BinaryOp, operand: Value) -> Result<Value, ExaError> {
		// check they're both numbers and unwrap them, or fail
		let (left, right) = match (self, operand) {
			(Value::Number(l), Value::Number(r)) => Ok((l, r)),
			_ => Err(ExaError::NotANumber),
		}?;
		let result = (match op {
			BinaryOp::Add => left + right,
			BinaryOp::Sub => left - right,
			// Multiplication can overflow i16, so be explicit about overflow behaviour
			BinaryOp::Mul => left.saturating_mul(right),
			BinaryOp::Div => left / right,
			BinaryOp::Mod => left % right,
			BinaryOp::Swiz => swiz(left, right),
		}).clamp(-9999, 9999);
		Ok(Value::Number(result))
	}
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
enum CommMode {
	Global,
	Local,
}

#[derive(Debug)]
struct File {
	id: u16,
	contents: Vec<Value>,
}

#[derive(Debug)]
struct HeldFile {
	file: File,
	position: u16,
}

impl HeldFile {
	fn read(&mut self) -> Result<Value, ExaError> {
		match self.file.contents.get(self.position as usize) {
			None => Err(ExaError::ReadEOF),
			Some(value) => {
				self.position += 1;
				Ok(*value)
			}
		}
	}

	fn write(&mut self, value: Value) -> Result<(), ExaError> {
		match self.file.contents.get_mut(self.position as usize) {
			None => {
				// Here's one you don't see much in normal gameplay:
				// Files have a 1000-value size limit.
				if self.file.contents.len() >= 1000 {
					Err(ExaError::FileIsFull)
				} else {
					self.file.contents.push(value);
					self.position += 1;
					Ok(())
				}
			},
			Some(slot) => {
				*slot = value;
				self.position += 1;
				Ok(())
			},
		}
	}

	fn delete(&mut self) -> Result<(), ExaError> {
		// Sadly, there's no option-returning version of remove() so we need to manually
		// check the index ourselves.
		if self.position as usize >= self.file.contents.len() {
			Err(ExaError::ReadEOF)
		} else {
			self.file.contents.remove(self.position as usize);
			Ok(())
		}
	}
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
enum Register {
	X, T, F, M,
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
enum RegOrValue {
	Register(Register),
	Value(Value),
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
enum BinaryOp {
	Add, Sub, Mul, Div, Mod, Swiz,
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
enum JumpCondition {
	None,
	True,
	False,
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
enum TestOp {
	Eq, Gt, Lt,
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
enum Instruction {
	// not implemented: rand, any redshift stuff
	Copy(RegOrValue, Register),
	BinaryOp(BinaryOp, RegOrValue, RegOrValue, Register),
	Jump(JumpCondition, u16),
	Test(TestOp, RegOrValue, RegOrValue),
	TestMRD,
	TestEOF,
	Repl(u16),
	Halt,
	Kill,
	Link(RegOrValue),
	Host(Register),
	Mode,
	VoidM,
	Make,
	Grab(RegOrValue),
	File(Register),
	Seek(RegOrValue),
	VoidF,
	Drop,
	Wipe,
	Noop,
}

// ExaErrors are reasons an exa can crash
#[derive(Debug,Clone,Copy,PartialEq,Eq)]
enum ExaError {
	NoHeldFile,
	ReadEOF,
	Killed,
	EndOfInstructions,
	NotANumber,
	DivideByZero,
	FileIsFull,
}

// Helper trait to define useful methods on Option<HeldFile>
trait HeldFileHelpers {
	// Helper function for dealing with operations on held files.
	// Returns a NoHeldFile error on None, or else calls the provided function
	// with a mutable reference to the unwrapped HeldFile.
	fn with<T, F>(&mut self, func: F) -> Result<T, ExaError>
	where F: FnOnce(&mut HeldFile) -> Result<T, ExaError>;
}

impl HeldFileHelpers for Option<HeldFile> {
	fn with<T, F>(&mut self, func: F) -> Result<T, ExaError>
	where F: FnOnce(&mut HeldFile) -> Result<T, ExaError> {
		match self {
			None => Err(ExaError::NoHeldFile),
			Some(held_file) => func(held_file),
		}
	}
}

#[derive(Debug)]
struct Exa<'a> {
	// General purpose registers
	reg_x: Value,
	reg_t: Value,
	// global or local comm mode
	comm_mode: CommMode,
	// code vec and the index into it that we will execute next
	code: &'a Vec<Instruction>,
	next_code_index: u16,
	// held file, if any
	file: Option<HeldFile>,
	// when the exa is dying (due to error or out of instrs),
	// the next execution is effectively a halt, and we are in a "zombie" state.
	// If we are dying, error is set.
	error: Option<ExaError>,
}

impl<'a> Exa<'a> {
	/// Create a new exa in basic initial state
	fn new(code: &Vec<Instruction>, comm_mode: CommMode) -> Exa {
		Exa {
			reg_x: Value::Number(0),
			reg_t: Value::Number(0),
			comm_mode: comm_mode,
			code: code,
			next_code_index: 0,
			file: None,
			error: None,
		}
	}

	/// Create a new exa as a REPL from an old one.
	/// Most state is transferred, but a new code index is picked
	/// and files are not copied.
	fn from_repl(parent: &Exa<'a>, code_index: u16) -> Self {
		Exa {
			next_code_index: code_index,
			file: None,
			.. *parent // otherwise inherits
		}
	}

	/// Run a single execution step.
	/// Returns either Ok or Err indicating the exa halted or errored.
	/// If the exa died holding a file, the file is handed back with the Err.
	fn step(mut self) -> Result<Self, Option<File>> {
		let instruction = match self.error {
			// We are dying, which implicitly means it's time to halt
			Some(_) => Instruction::Halt,
			None => {
				if self.next_code_index as usize >= self.code.len() {
					// We are at end of code. This is a weird case because normally
					// we detect this condition the step before and we're already dying by now.
					// However we can reach this state after a jump or repl to end of code.
					// Just insert an implicit Noop. Yes, this means it takes an extra cycle
					// to die. Yes, this is accurate to the game.
					Instruction::Noop
				} else {
					// Otherwise, fetch instruction and increment code index
					let instruction = self.code[self.next_code_index as usize];
					self.next_code_index += 1;
					instruction
				}
			}
		};

		// We need to know if we've just jumped to handle a special case below
		let mut jumped = false;

		let result: Result<(), ExaError> = match instruction {
			Instruction::Copy(src, dest) => {
				self.read_reg_or_value(src)
					.and_then(|value| self.write_reg(dest, value))
			},
			Instruction::BinaryOp(op, left, right, dest) => {
				self.read_reg_or_value(left) // attempt to read left operand
					.and_then(|left|
						// if left succeeded, attempt to read right,
						// and pass both onwards
						self.read_reg_or_value(right).map(|right| (left, right))
					).and_then(|(left, right)|
						// actually do the op, which may fail for non-numbers
						left.operate(op, right)
					).and_then(|result|
						// finally, attempt to write it
						self.write_reg(dest, result)
					)
			},
			Instruction::Jump(condition, target) => {
				let do_jump = match condition {
					JumpCondition::None => false,
					JumpCondition::True => self.reg_t != Value::Number(0),
					JumpCondition::False => self.reg_t == Value::Number(0),
				};
				if do_jump {
					self.next_code_index = target;
					jumped = true;
				}
				Ok(())
			},
			Instruction::Test(TestOp, RegOrValue, _RegOrValue) => unimplemented!(),
			Instruction::TestMRD => unimplemented!(),
			Instruction::TestEOF => {
				let reg_t = &mut self.reg_t; // be specific to avoid borrowing self inside closure
				self.file.with(|held_file| {
					*reg_t = if held_file.position as usize >= held_file.file.contents.len() {
						Value::Number(1)
					} else {
						Value::Number(0)
					};
					Ok(())
				})
			},
			Instruction::Repl(i16) => unimplemented!(),
			Instruction::Halt => {
				// Special case: Stop immediately. Drop any held file.
				return Err(self.file.map(|held_file| held_file.file));
			},
			Instruction::Kill => unimplemented!(),
			Instruction::Link(RegOrValue) => unimplemented!(),
			Instruction::Host(Register) => unimplemented!(),
			Instruction::Mode => {
				self.comm_mode = match self.comm_mode {
					CommMode::Local => CommMode::Global,
					CommMode::Global => CommMode::Local,
				};
				Ok(())
			},
			Instruction::VoidM => unimplemented!(),
			Instruction::Make => unimplemented!(),
			Instruction::Grab(RegOrValue) => unimplemented!(),
			Instruction::File(register) => {
				self.file.with(|held_file| self.write_reg(register, Value::Number(held_file.file.id as i16)))
			},
			Instruction::Seek(reg_or_value) => {
				// There is an important subtlety here.
				// If an exa does a "SEEK M" while not holding a file,
				// it *still reads a value from M* before dying.
				// Less importantly, if the value is a symbol, it fails with NotANumber, not NoHeldFile.
				// So our order of operations must be: read reg, then check value, then check file.
				self.read_reg_or_value(reg_or_value).and_then(|value| {
					match value {
						Value::Symbol(_) => Err(ExaError::NotANumber),
						Value::Number(offset) => self.file.with(|held_file| {
							// We need to hack around rust's strict integer type operations here.
							// We convert everything to i16 (safe since our values are in [0,1000]
							// and [-9999, 9999] respectively), do the addition, then clamp back
							// down to [0, len] and convert back to u16
							held_file.position = (held_file.position as i16 + offset).clamp(
								0, held_file.file.contents.len() as i16 // note 1 past end of vec is allowed
							) as u16;
							Ok(())
						})
					}
				})
			},
			Instruction::VoidF => {
				self.file.with(|held_file| { held_file.delete() })
			},
			Instruction::Drop => unimplemented!(),
			Instruction::Wipe => {
				match self.file {
					None => Err(ExaError::NoHeldFile),
					Some(held_file) => {
						self.file = None;
						Ok(())
					}
				}
			},
			Instruction::Noop => Ok(()),
		};

		// Check for errors
		self.error = if let Err(error) = result {
			// crashed due to instruction
			Some(error)
		} else if (!jumped) && self.next_code_index as usize == self.code.len() {
			// reached end of instructions. note we ignore this if we jumped this cycle,
			// as in this case the exa doesn't start dying for another cycle.
			Some(ExaError::EndOfInstructions)
		} else {
			None
		};

		Ok(self)
	}

	/// Trigger exa to die in following step
	fn kill(&mut self) {
		self.error = Some(ExaError::Killed);
	}

	fn write_reg(&mut self, reg: Register, value: Value) -> Result<(), ExaError> {
		match reg {
			Register::X => { self.reg_x = value; Ok(()) },
			Register::T => { self.reg_t = value; Ok(()) },
			Register::F => self.file.with(|held_file| held_file.write(value)),
			Register::M => unimplemented!(),
		}
	}

	fn read_reg(&mut self, reg: Register) -> Result<Value, ExaError> {
		match reg {
			Register::X => Ok(self.reg_x),
			Register::T => Ok(self.reg_t),
			Register::F => self.file.with(|held_file| held_file.read()),
			Register::M => unimplemented!(),
		}
	}

	fn read_reg_or_value(&mut self, reg_or_value: RegOrValue) -> Result<Value, ExaError> {
		match reg_or_value {
			RegOrValue::Register(reg) => self.read_reg(reg),
			RegOrValue::Value(value) => Ok(value),
		}
	}

}

// Implements the SWIZ operation:
//  For each digit i in right, same digit of result = ith digit of left (count digits LSD to MSD, 1-based).
//  Sign of result = multiply sign of inputs (same sign -> positive, diff sign -> negative).
fn swiz(left: i16, right: i16) -> i16 {
	fn get_digit(value: i16, digit: i16) -> i16 {
		(value.abs() / 10_i16.pow(digit as u32 - 1)) % 10
	}

	let mut result: i16 = 0;
	for right_digit in 1..4 {
		let left_digit = get_digit(right, right_digit);
		result += get_digit(left, left_digit) * 10_i16.pow(right_digit as u32 - 1)
	}

	result
}
