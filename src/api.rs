use axum::Router;

pub mod index;
pub mod b;

pub fn router() -> Router {
    // This is the order that the modules were authored in.
    index::router()
    .merge(b::router())
}
