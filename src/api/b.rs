use axum::{Router, routing::get, response::IntoResponse};
use proto::{UNAUTH_DEFAULT, auth::User};

pub fn router() -> Router {
  Router::new()
    .route("/b", get(b))
}

pub async fn b(user: Option<User>) -> impl IntoResponse {
  match user {
    Some(_u) => "buffer",
    None => UNAUTH_DEFAULT,
  }
}
