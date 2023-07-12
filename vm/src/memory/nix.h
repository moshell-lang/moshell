#pragma once

#include <vector>

struct redir {
    int back_fd;
    int target_fd;
};

class fd_table {
private:
    std::vector<redir> active_redirections;

public:
    int push_redirection(int from_fd, int to_fd);
    void pop_redirection();
};
