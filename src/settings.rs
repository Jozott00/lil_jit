use std::sync::atomic::{AtomicBool, Ordering};

pub static JIT_SETTINGS: JitSettings = JitSettings {
    const_opt: AtomicBool::new(true),
};

pub struct JitSettings {
    const_opt: AtomicBool,
}

impl JitSettings {
    pub fn set_const_opt(&self, val: bool) {
        self.const_opt.store(val, Ordering::Relaxed);
    }

    pub fn const_opt(&self) -> bool {
        self.const_opt.fetch_and(true, Ordering::Relaxed)
    }
}
