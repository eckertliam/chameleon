// the entirety of Chameleon's type system lives here

use std::collections::HashMap;

pub struct TypeEnv {
    types: HashMap<String, Box<dyn Type>>,
}

trait Type {
    /// returns the size of the type in bytes
    fn size(&self) -> u8;
}

struct IntType {
    size: u8,
    signed: bool,
}

impl IntType {
    pub fn i8() -> IntType {
        IntType {
            size: 1,
            signed: true,
        }
    }

    pub fn i16() -> IntType {
        IntType {
            size: 2,
            signed: true,
        }
    }

    pub fn i32() -> IntType {
        IntType {
            size: 4,
            signed: true,
        }
    }

    pub fn i64() -> IntType {
        IntType {
            size: 8,
            signed: true,
        }
    }

    pub fn u8() -> IntType {
        IntType {
            size: 1,
            signed: false,
        }
    }

    pub fn u16() -> IntType {
        IntType {
            size: 2,
            signed: false,
        }
    }

    pub fn u32() -> IntType {
        IntType {
            size: 4,
            signed: false,
        }
    }

    pub fn u64() -> IntType {
        IntType {
            size: 8,
            signed: false,
        }
    }
}

impl Type for IntType {
    fn size(&self) -> u8 {
        self.size
    }
}

struct FloatType {
    bytes: u8,
}

impl FloatType {
    pub fn f32() -> FloatType {
        FloatType { bytes: 4 }
    }

    pub fn f64() -> FloatType {
        FloatType { bytes: 8 }
    }
}

impl Type for FloatType {
    fn size(&self) -> u8 {
        self.bytes
    }
}