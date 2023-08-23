#![recursion_limit = "1024"]
#[cfg(not(debug_assertions))]
const LOG_LEVEL: log::Level = log::Level::Info;
#[cfg(debug_assertions)]
const LOG_LEVEL: log::Level = log::Level::Trace;

fn main() {
  wasm_logger::init(wasm_logger::Config::new(LOG_LEVEL));
  log::info!("greetings, stranger.");
}
