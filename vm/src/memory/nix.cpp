#include "nix.h"

#include <unistd.h>

int fd_table::push_redirection(int from_fd, int to_fd) {
    int back_fd = dup(to_fd);
    if (back_fd == -1) {
        return -1;
    }
    if (dup2(from_fd, to_fd) == -1) {
        close(back_fd);
        return -1;
    }
    active_redirections.push_back({back_fd, to_fd});
    return 0;
}

void fd_table::pop_redirection() {
    redir r = active_redirections.back();
    active_redirections.pop_back();
    dup2(r.back_fd, r.target_fd);
    close(r.back_fd);
}
