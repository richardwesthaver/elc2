use el::{
  cfg::Cfg,
  app::map_org_dir,
  api::index::index,
};

use clap::Parser;
use anyhow::Result;
use proto::{
  UNAUTH_DEFAULT,
  error::internal_error,
  auth::{User, MemoryStore},
};

use axum::{
  extract::Extension,
  routing::{get, get_service},
  Router,
};
use std::net::SocketAddr;
use tower_http::{services::ServeFile, trace::TraceLayer};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[tokio::main]
async fn main() -> Result<()> {
  tracing_subscriber::registry()
    .with(tracing_subscriber::EnvFilter::new(
      std::env::var("RUST_LOG")
        .unwrap_or_else(|_| "elsrv=debug,tower_http=debug".into()),
        ))
    .with(tracing_subscriber::fmt::layer())
    .init();

  let config = Cfg::parse();
  let org_path = config.org_path;
  let mut org_files: Vec<String> = vec![];
  map_org_dir(&org_path, &mut org_files)?;
  
  let mut print_files = String::new();
  for i in org_files.iter() {
    print_files.push_str(&format!("{}\n", i));
  }
  let addr: SocketAddr = config.elsrv_addr.parse()?;
  println!("listening on {}", addr);
  println!("/org -- {}", &org_path.display());
  println!("org files found:");
  println!("{}", &print_files);
  let oauth_store = MemoryStore::new();
  let mut app = Router::new()
    .route("/", get(index))
    .route("/org", get(|user: Option<User>| async
		       {
			 match user {
			   Some(_u) => print_files,
			   None => UNAUTH_DEFAULT.to_string(),
			 }
		       }
    ))
//    .fallback(get_service(ServeDir::new(org_path)).handle_error(internal_error))
    .layer(TraceLayer::new_for_http())
    .layer(Extension(oauth_store));

  for i in org_files.iter() {
    app = app.route(&format!("{}",i.strip_prefix("..").unwrap()), get_service(ServeFile::new(i)).handle_error(internal_error));
  }
  axum::Server::bind(&addr)
    .serve(app.into_make_service())
    .await
    .unwrap();
  Ok(())
}

