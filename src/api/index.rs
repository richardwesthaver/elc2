use axum::{Router, routing::get, response::{Html, IntoResponse}};
#[cfg(feature="auth")]
use proto::UNAUTH_DEFAULT;
use proto::auth::User;

pub fn router() -> Router {
  Router::new()
    .route("/", get(index))
}

// Session is optional
pub async fn index(user: Option<User>) -> impl IntoResponse {
    match user {
      #[cfg(feature="auth")]
      Some(u) => Html(format!(
        "operator: {:?}
/el -- execute elisp
/b -- view buffers",
        u,
      )),
      #[cfg(feature="auth")]
      None => Html(UNAUTH_DEFAULT.to_string()),
      #[cfg(not(feature="auth"))]
      _ => Html("operator: nil
/el -- execute elisp
/b -- view buffers",
      )
    }
}

//  TODO 2022-07-29: implement org handler
//    - this requires that the file list is available in a store so that
//      it can be accessed
//    - requires auth (can make a macro for this condition)
//    - make sure all files are unaccessible without auth
pub async fn org_index(
  user: Option<User>) -> impl IntoResponse {
  match user {
    #[cfg(feature="auth")]
    Some(_) => "".to_string(),
    #[cfg(feature="auth")]
    None => UNAUTH_DEFAULT.to_string(),
    #[cfg(not(feature="auth"))]
    _ => "org_index".to_string(),
  }
}
