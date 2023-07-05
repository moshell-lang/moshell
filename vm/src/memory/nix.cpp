#include "nix.h"

#include <unistd.h>

void fd_table::push_redirection(int from_fd, int to_fd) {
    int back_fd = dup(to_fd);
    dup2(from_fd, to_fd);
    active_redirections.push_back({back_fd, to_fd});
}

void fd_table::pop_redirection() {
    redir r = active_redirections.back();
    active_redirections.pop_back();
    dup2(r.back_fd, r.target_fd);
    close(r.back_fd);
}
