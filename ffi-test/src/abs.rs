use std::collections::HashMap;
use std::error::Error;
use std::ffi::{CStr, c_char, c_double, c_uchar, c_void};
use std::sync::{Arc, Mutex};

static mut ABSOLUTESOLVER_RTS_INITIALIZED: bool = false;

unsafe extern "C" {
    fn absoluteSolverInit() -> c_uchar;
    fn absoluteSolverExit();

    fn solvedForHs(eqn: *const c_char, target: *const c_char) -> *mut c_char;

    fn solvedForValueHs(
        eqn: *const c_char,
        target: *const c_char,
        context: *const c_char,
    ) -> c_double;

    fn free(p: *const c_void);
}

pub struct AbsoluteSolver {}
impl Drop for AbsoluteSolver {
    fn drop(&mut self) {
        unsafe {
            absoluteSolverExit();
        }
    }
}
impl AbsoluteSolver {
    fn solve_for(&self, eqn: &str, target: &str) -> Result<String, Box<dyn Error>> {
        let eqn_p = eqn.as_ptr() as *const c_char;
        let target_p = target.as_ptr() as *const c_char;
        let result: String;

        unsafe {
            let soln_p = solvedForHs(eqn_p, target_p);
            if soln_p.is_null() {
                return Err(Box::new());
            }
            let soln_str = CStr::from_ptr(soln_p);
            result = soln_str.to_str()?.to_string();
            free(soln_p as *const c_void);
        }

        Ok(result)
    }

    fn solve_for_value(
        &self,
        eqn: &str,
        target: &str,
        context: HashMap<&str, f64>,
    ) -> Result<f64, Box<dyn Error>> {
        let result: f64 = unsafe {
            solvedForValueHs(
                eqn.as_ptr() as *const c_char,
                target.as_ptr() as *const c_char,
                context
                    .iter()
                    .map(|(k, v)| format!("{k}={v}"))
                    .collect::<Vec<String>>()
                    .join(",")
                    .as_ptr() as *const c_char,
            )
        };

        Ok(result)
    }
}
