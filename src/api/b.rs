use axum::{Router, routing::get, response::IntoResponse};
#[cfg(feature="auth")]
use proto::UNAUTH_DEFAULT;
use proto::auth::User;

pub fn router() -> Router {
  Router::new()
    .route("/b", get(b))
}

pub async fn b(user: Option<User>) -> impl IntoResponse {
  match user {
    #[cfg(feature="auth")]
    Some(_u) => "buffer",
    #[cfg(feature="auth")]
    None => UNAUTH_DEFAULT,
    #[cfg(not(feature="auth"))]
    _ => "buffer",
  }
}
