

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
	fn operate(&self, op: BinaryOp, operand: Value) -> Result<Value, &'static str> {
		// check they're both numbers and unwrap them, or fail
		let left, right = match (self, operand) {
			(Value::Number(l), Value::Number(r)) => Ok((l, r)),
			_ => Err("Cannot perform arithmetic on symbols"),
		}?
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

#[derive(Debug, PartialEq, Eq)]
enum Register {
	X, T, F, M,
}

#[derive(Debug)]
enum RegOrValue {
	Register(Register),
	Value(Value,
}

#[derive(Debug)]
enum BinaryOp {
	Add, Sub, Mul, Div, Mod, Swiz,
}

#[derive(Debug)]
enum JumpCondition {
	None,
	True,
	False,
}

#[derive(Debug)]
enum TestOp {
	Eq, Gt, Lt,
}

#[derive(Debug)]
enum Instruction {
	// not implemented: rand, any redshift stuff
	Copy(RegOrValue, Register),
	BinaryOp(BinaryOp, RegOrValue, RegOrValue, Register),
	Jump(JumpCondition, i16),
	Test(TestOp, RegOrValue, RegOrValue),
	TestMRD,
	TestEOF,
	Repl(i16),
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
	// the next execution is effectively a halt.
	// If we are dying, death_message is set.
	// Death messages are only ever string literals for now.
	// Later we can probably replace with a DeathReason enum.
	death_message: Option<&'static str>,
}

impl Exa {
	/// Create a new exa in basic initial state
	fn new(code: &Vec<Instruction>, comm_mode: CommMode) -> Exa {
		Exa {
			reg_x: Value::Number(0),
			reg_t: Value::Number(0),
			comm_mode: comm_mode,
			code: code,
			next_code_index: 0,
			file: None,
			death_message: None,
		}
	}

	/// Create a new exa as a REPL from an old one.
	/// Most state is transferred, but a new code index is picked
	/// and files are not copied.
	fn from_repl(parent: &Exa, code_index: u16) -> Self {
		Exa {
			next_code_index: code_index,
			file: None,
			death_message: None, // parent might already be dying if repl was last instr
			.. parent // otherwise inherits
		}
	}

	/// Run a single execution step.
	/// Returns either Ok or Err indicating the exa halted or errored.
	/// If the exa died holding a file, the file is handed back with the Err.
	fn step(self) -> Result<Self, Option<File>> {
		let instruction = match self.death_message {
			// We are dying, which implicitly means it's time to halt
			Some(_) => Instruction::Halt,
			None => {
				let instruction;
				if self.next_code_index >= self.code.len() {
					// We are at end of code. This is a weird case because normally
					// we detect this condition the step before and we're already dying by now.
					// However we can reach this state after a jump or repl to end of code.
					// Just insert an implicit Noop. Yes, this means it takes an extra cycle
					// to die. Yes this is accurate to the game.
					instruction = Instruction::Noop
				} else {
					// Otherwise, fetch instruction and increment code index
					let instruction = self.code[self.next_code_index];
					self.next_code_index += 1;
				}
				// And start dying if we are now at end of code.
				// If we jump away on this instruction we'll clear this later.
				if self.next_code_index >= self.code.len() {
					self.death_message = Some("End of Instructions");
				}
				instruction
			}
		};

		let result = match instruction {
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
						left.operate(right)
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
					// It's possible a death message was set due to end of code.
					// Since we're jumping away (possibly to end of code anyway, but
					// the game takes an extra cycle in this case) we need to clear it.
					self.death_message = None;
				}
				Ok(())
			},
			Instruction::Test(TestOp, RegOrValue, RegOrValue),
			Instruction::TestMRD,
			Instruction::TestEOF => {
				match self.file {
					None => Err("No held file"),
					Some(held_file) => {
						self.reg_t = if held_file.position >= held_file.file.contents.len() {
							Value::Number(1)
						} else {
							Value::Number(0)
						}
						Ok(())
					}
				}
			},
			Instruction::Repl(i16),
			Instruction::Halt,
			Instruction::Kill,
			Instruction::Link(RegOrValue),
			Instruction::Host(Register),
			Instruction::Mode => {
				self.comm_mode = match self.comm_mode {
					CommMode::Local => CommMode::Global,
					CommMode::Global => CommMode::Local,
				};
				Ok(())
			},
			Instruction::VoidM,
			Instruction::Make,
			Instruction::Grab(RegOrValue),
			Instruction::File(register) => {
				match self.file {
					None => Err("No held file"),
					Some(held_file) => self.write_reg(register, Number(held_file.file.id as i16)),
				}
			},
			Instruction::Seek(reg_or_value) => {
				self.read_reg_or_value(reg_or_value).and_then(|value| {
					match value {
						Value::Symbol(_) => Err("Cannot seek with symbol"),
						Value::Number(offset) => match self.file {
							None => Err("No held file"),
							Some(held_file) => {
								held_file.position = (held_file.position + offset).clamp(
									0, held_file.file.contents.len() // note 1 past end of vec is allowed
								);
								Ok(()),
							}
						}
					}
				})
			},
			Instruction::VoidF,
			Instruction::Drop,
			Instruction::Wipe => {
				match self.file {
					None => Err("No held file"),
					Some(held_file) => {
						held_file.file.contents.clear();
						held_file.position = 0;
						Ok(());
					}
				}
			},
			Instruction::Noop => Ok(()),
		};
		Ok(self)
	}

	/// Trigger exa to die in following step with given error message
	fn die(&mut self, reason: &'static str) {
		self.death_message = Some(reason);
	}

	fn write_reg(&mut self, reg: Register, value: Value) {
		match reg {
			Register::X => { self.reg_x = value; },
			Register::T => { self.reg_t = value; },
			Register::F => {
				if let Some(file) = self.file {
					file.write(value)
				} else {
					self.die("Write to file with none held");
				};
			},
			Register::M => unimplemented!(),
		};
	}

	fn read_reg(&mut self, reg: Register) -> Result<Value, ()> {
		match reg {
			Register::X => Ok(self.reg_x),
			Register::T => Ok(self.reg_t),
			Register::F => {
				if let Some(file) = self.file {
					file.read()
				} else {
					self.die("Read of file with none held");
					Err()
				}
			},
			Register::M => unimplemented!(),
		}
	}

	fn read_reg_or_value(&mut self, reg_or_value: RegOrValue) -> Result<Value, ()> {
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
		((value.abs() / 10.pow(digit - 1)) % 10)
	}

	let mut result: i16 = 0;
	for right_digit in 1..4 {
		let left_digit = get_digit(right, right_digit);
		result += get_digit(left, left_digit) * 10.pow(right_digit - 1)
	}

	result
}
