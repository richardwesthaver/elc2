use crate::StdResult;

use std::{
  net::SocketAddr,
  path::{Path, PathBuf},
  fs, io,
};

#[derive(Debug)]
pub struct ClientState {
  socket: SocketAddr,
//  user_id: 
}


#[derive(Debug)]
pub enum ServerStatus {
  Up,
  Down,
}

#[derive(Debug)]
pub struct ServerState {
  status: ServerStatus,
  rx: SocketAddr,
  tx: SocketAddr,
  clients: Vec<ClientState>,
  location: PathBuf,
}

impl Default for ServerState {
  fn default() -> Self {
    ServerState {
      status: ServerStatus::Down,
      rx: "127.0.0.1:8000".parse().unwrap(),
      tx: "127.0.0.1:3000".parse().unwrap(),
      clients: vec![],
      location: PathBuf::from("."),
    }
  }
}
pub struct Server {
  state: ServerState,
}

impl Server {
  pub fn new() -> Server {
    Server {
      state: ServerState::default(),
    }
  }
}

pub fn map_org_dir<P: AsRef<Path>>(path: P, mut coll: &mut Vec<String>) -> StdResult<(), io::Error> {
  let path = path.as_ref();
  if path.is_dir() {
    for elt in fs::read_dir(path)? {
      let elt = elt?;
      let p = elt.path();
      if p.is_dir() {
        map_org_dir(p, &mut coll)?;
      } else if p.is_file() {
        coll.push(p.display().to_string())
      }
    }
  } else if path.is_file() {
      coll.push(path.display().to_string())
  }
  Ok(())
}

