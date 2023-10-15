use directories::ProjectDirs;

pub mod pipeline;

pub fn project_dir() -> Option<ProjectDirs> {
    ProjectDirs::from("", "", "moshell")
}
