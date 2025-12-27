/*
 * Citadel Socket FFI
 * Simple TCP socket bindings using POSIX sockets
 */

#include <lean/lean.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>

/* Socket handle - just wraps a file descriptor */
typedef struct {
    int fd;
} citadel_socket_t;

static lean_external_class *g_socket_class = NULL;

static void citadel_socket_finalizer(void *ptr) {
    citadel_socket_t *sock = (citadel_socket_t *)ptr;
    if (sock->fd >= 0) {
        close(sock->fd);
    }
    free(sock);
}

static void citadel_socket_foreach(void *ptr, b_lean_obj_arg f) {
    /* No nested Lean objects */
}

static inline lean_obj_res citadel_socket_box(citadel_socket_t *sock) {
    if (g_socket_class == NULL) {
        g_socket_class = lean_register_external_class(
            citadel_socket_finalizer,
            citadel_socket_foreach
        );
    }
    return lean_alloc_external(g_socket_class, sock);
}

static inline citadel_socket_t *citadel_socket_unbox(lean_obj_arg obj) {
    return (citadel_socket_t *)lean_get_external_data(obj);
}

/* Create a new TCP socket */
LEAN_EXPORT lean_obj_res citadel_socket_new(lean_obj_arg world) {
    citadel_socket_t *sock = malloc(sizeof(citadel_socket_t));
    if (!sock) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to allocate socket")));
    }

    sock->fd = socket(AF_INET, SOCK_STREAM, 0);
    if (sock->fd < 0) {
        free(sock);
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string(strerror(errno))));
    }

    /* Set SO_REUSEADDR */
    int opt = 1;
    setsockopt(sock->fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    /* Set recv/send timeouts to 5 seconds */
    struct timeval timeout;
    timeout.tv_sec = 5;
    timeout.tv_usec = 0;
    setsockopt(sock->fd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));
    setsockopt(sock->fd, SOL_SOCKET, SO_SNDTIMEO, &timeout, sizeof(timeout));

    return lean_io_result_mk_ok(citadel_socket_box(sock));
}

/* Bind socket to address */
LEAN_EXPORT lean_obj_res citadel_socket_bind(
    b_lean_obj_arg sock_obj,
    b_lean_obj_arg host,
    uint16_t port,
    lean_obj_arg world
) {
    citadel_socket_t *sock = citadel_socket_unbox(sock_obj);
    const char *host_str = lean_string_cstr(host);

    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);

    if (inet_pton(AF_INET, host_str, &addr.sin_addr) <= 0) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Invalid address")));
    }

    if (bind(sock->fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string(strerror(errno))));
    }

    return lean_io_result_mk_ok(lean_box(0));
}

/* Listen for connections */
LEAN_EXPORT lean_obj_res citadel_socket_listen(
    b_lean_obj_arg sock_obj,
    uint32_t backlog,
    lean_obj_arg world
) {
    citadel_socket_t *sock = citadel_socket_unbox(sock_obj);

    if (listen(sock->fd, backlog) < 0) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string(strerror(errno))));
    }

    return lean_io_result_mk_ok(lean_box(0));
}

/* Accept a connection */
LEAN_EXPORT lean_obj_res citadel_socket_accept(
    b_lean_obj_arg sock_obj,
    lean_obj_arg world
) {
    citadel_socket_t *sock = citadel_socket_unbox(sock_obj);

    struct sockaddr_in client_addr;
    socklen_t addr_len = sizeof(client_addr);

    int client_fd = accept(sock->fd, (struct sockaddr *)&client_addr, &addr_len);
    if (client_fd < 0) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string(strerror(errno))));
    }

    citadel_socket_t *client = malloc(sizeof(citadel_socket_t));
    if (!client) {
        close(client_fd);
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to allocate client socket")));
    }
    client->fd = client_fd;

    /* Set recv/send timeouts to 5 seconds on client socket */
    struct timeval timeout;
    timeout.tv_sec = 5;
    timeout.tv_usec = 0;
    setsockopt(client_fd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));
    setsockopt(client_fd, SOL_SOCKET, SO_SNDTIMEO, &timeout, sizeof(timeout));

    return lean_io_result_mk_ok(citadel_socket_box(client));
}

/* Receive data */
LEAN_EXPORT lean_obj_res citadel_socket_recv(
    b_lean_obj_arg sock_obj,
    uint32_t max_bytes,
    lean_obj_arg world
) {
    citadel_socket_t *sock = citadel_socket_unbox(sock_obj);

    uint8_t *buffer = malloc(max_bytes);
    if (!buffer) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to allocate buffer")));
    }

    ssize_t n = recv(sock->fd, buffer, max_bytes, 0);
    if (n < 0) {
        free(buffer);
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string(strerror(errno))));
    }

    lean_obj_res arr = lean_alloc_sarray(1, n, n);
    memcpy(lean_sarray_cptr(arr), buffer, n);
    free(buffer);

    return lean_io_result_mk_ok(arr);
}

/* Send data */
LEAN_EXPORT lean_obj_res citadel_socket_send(
    b_lean_obj_arg sock_obj,
    b_lean_obj_arg data,
    lean_obj_arg world
) {
    citadel_socket_t *sock = citadel_socket_unbox(sock_obj);

    size_t len = lean_sarray_size(data);
    const uint8_t *ptr = lean_sarray_cptr(data);

    size_t sent = 0;
    while (sent < len) {
        ssize_t n = send(sock->fd, ptr + sent, len - sent, 0);
        if (n < 0) {
            return lean_io_result_mk_error(lean_mk_io_user_error(
                lean_mk_string(strerror(errno))));
        }
        sent += n;
    }

    return lean_io_result_mk_ok(lean_box(0));
}

/* Close socket */
LEAN_EXPORT lean_obj_res citadel_socket_close(
    lean_obj_arg sock_obj,
    lean_obj_arg world
) {
    citadel_socket_t *sock = citadel_socket_unbox(sock_obj);

    if (sock->fd >= 0) {
        close(sock->fd);
        sock->fd = -1;
    }

    lean_dec_ref(sock_obj);
    return lean_io_result_mk_ok(lean_box(0));
}

/* Get socket file descriptor (for debugging) */
LEAN_EXPORT uint32_t citadel_socket_fd(b_lean_obj_arg sock_obj) {
    citadel_socket_t *sock = citadel_socket_unbox(sock_obj);
    return (uint32_t)sock->fd;
}
