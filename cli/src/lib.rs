use directories::ProjectDirs;

pub fn project_dir() -> Option<ProjectDirs> {
    ProjectDirs::from("", "", "moshell")
}
