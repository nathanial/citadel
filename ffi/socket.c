/*
 * Citadel TLS Socket FFI
 * TLS socket bindings using OpenSSL
 * (Plain TCP sockets now provided by Jack)
 */

#include <lean/lean.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <openssl/ssl.h>
#include <openssl/err.h>

/* TLS socket handle */
typedef struct {
    int fd;
    SSL_CTX *ctx;  /* Owned by server socket, NULL for client sockets */
    SSL *ssl;      /* Per-connection SSL state */
} citadel_tls_socket_t;

static lean_external_class *g_tls_socket_class = NULL;

static void citadel_tls_socket_finalizer(void *ptr) {
    citadel_tls_socket_t *sock = (citadel_tls_socket_t *)ptr;
    if (sock->ssl) {
        SSL_shutdown(sock->ssl);
        SSL_free(sock->ssl);
    }
    if (sock->ctx) {
        SSL_CTX_free(sock->ctx);
    }
    if (sock->fd >= 0) {
        close(sock->fd);
    }
    free(sock);
}

static void citadel_tls_socket_foreach(void *ptr, b_lean_obj_arg f) {
    /* No nested Lean objects */
}

static inline lean_obj_res citadel_tls_socket_box(citadel_tls_socket_t *sock) {
    if (g_tls_socket_class == NULL) {
        g_tls_socket_class = lean_register_external_class(
            citadel_tls_socket_finalizer,
            citadel_tls_socket_foreach
        );
    }
    return lean_alloc_external(g_tls_socket_class, sock);
}

static inline citadel_tls_socket_t *citadel_tls_socket_unbox(lean_obj_arg obj) {
    return (citadel_tls_socket_t *)lean_get_external_data(obj);
}

/* Helper to get OpenSSL error string */
static char *get_ssl_error_string(void) {
    static char buf[256];
    unsigned long err = ERR_get_error();
    if (err) {
        ERR_error_string_n(err, buf, sizeof(buf));
        return buf;
    }
    return "Unknown SSL error";
}

/* Create a new TLS server socket */
LEAN_EXPORT lean_obj_res citadel_tls_socket_new_server(
    b_lean_obj_arg cert_file,
    b_lean_obj_arg key_file,
    lean_obj_arg world
) {
    const char *cert_path = lean_string_cstr(cert_file);
    const char *key_path = lean_string_cstr(key_file);

    /* Initialize OpenSSL (safe to call multiple times in OpenSSL 1.1+) */
    SSL_library_init();
    SSL_load_error_strings();

    /* Create SSL context for TLS server */
    SSL_CTX *ctx = SSL_CTX_new(TLS_server_method());
    if (!ctx) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string(get_ssl_error_string())));
    }

    /* Set minimum TLS version to TLS 1.2 */
    SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION);

    /* Load certificate */
    if (SSL_CTX_use_certificate_file(ctx, cert_path, SSL_FILETYPE_PEM) <= 0) {
        SSL_CTX_free(ctx);
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string(get_ssl_error_string())));
    }

    /* Load private key */
    if (SSL_CTX_use_PrivateKey_file(ctx, key_path, SSL_FILETYPE_PEM) <= 0) {
        SSL_CTX_free(ctx);
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string(get_ssl_error_string())));
    }

    /* Verify private key matches certificate */
    if (SSL_CTX_check_private_key(ctx) != 1) {
        SSL_CTX_free(ctx);
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Private key does not match certificate")));
    }

    /* Create underlying TCP socket */
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) {
        SSL_CTX_free(ctx);
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string(strerror(errno))));
    }

    /* Set SO_REUSEADDR */
    int opt = 1;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    /* Create TLS socket structure */
    citadel_tls_socket_t *sock = malloc(sizeof(citadel_tls_socket_t));
    if (!sock) {
        close(fd);
        SSL_CTX_free(ctx);
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to allocate TLS socket")));
    }

    sock->fd = fd;
    sock->ctx = ctx;
    sock->ssl = NULL;  /* Server socket doesn't have SSL yet */

    return lean_io_result_mk_ok(citadel_tls_socket_box(sock));
}

/* Bind TLS socket to address */
LEAN_EXPORT lean_obj_res citadel_tls_socket_bind(
    b_lean_obj_arg sock_obj,
    b_lean_obj_arg host,
    uint16_t port,
    lean_obj_arg world
) {
    citadel_tls_socket_t *sock = citadel_tls_socket_unbox(sock_obj);
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

/* Listen for TLS connections */
LEAN_EXPORT lean_obj_res citadel_tls_socket_listen(
    b_lean_obj_arg sock_obj,
    uint32_t backlog,
    lean_obj_arg world
) {
    citadel_tls_socket_t *sock = citadel_tls_socket_unbox(sock_obj);

    if (listen(sock->fd, backlog) < 0) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string(strerror(errno))));
    }

    return lean_io_result_mk_ok(lean_box(0));
}

/* Accept a TLS connection and perform handshake */
LEAN_EXPORT lean_obj_res citadel_tls_socket_accept(
    b_lean_obj_arg sock_obj,
    lean_obj_arg world
) {
    citadel_tls_socket_t *server = citadel_tls_socket_unbox(sock_obj);

    /* Accept TCP connection */
    struct sockaddr_in client_addr;
    socklen_t addr_len = sizeof(client_addr);

    int client_fd = accept(server->fd, (struct sockaddr *)&client_addr, &addr_len);
    if (client_fd < 0) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string(strerror(errno))));
    }

    /* Set timeouts on client socket */
    struct timeval timeout;
    timeout.tv_sec = 10;  /* 10 seconds for TLS handshake */
    timeout.tv_usec = 0;
    setsockopt(client_fd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));
    setsockopt(client_fd, SOL_SOCKET, SO_SNDTIMEO, &timeout, sizeof(timeout));

    /* Create SSL object for this connection */
    SSL *client_ssl = SSL_new(server->ctx);
    if (!client_ssl) {
        close(client_fd);
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string(get_ssl_error_string())));
    }

    /* Associate SSL with socket */
    SSL_set_fd(client_ssl, client_fd);

    /* Perform TLS handshake */
    int ret = SSL_accept(client_ssl);
    if (ret != 1) {
        int err = SSL_get_error(client_ssl, ret);
        char errbuf[256];
        snprintf(errbuf, sizeof(errbuf), "TLS handshake failed: %s (SSL error %d)",
                 get_ssl_error_string(), err);
        SSL_free(client_ssl);
        close(client_fd);
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string(errbuf)));
    }

    /* Reduce timeout for normal operations */
    timeout.tv_sec = 5;
    setsockopt(client_fd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));
    setsockopt(client_fd, SOL_SOCKET, SO_SNDTIMEO, &timeout, sizeof(timeout));

    /* Create client TLS socket */
    citadel_tls_socket_t *client = malloc(sizeof(citadel_tls_socket_t));
    if (!client) {
        SSL_shutdown(client_ssl);
        SSL_free(client_ssl);
        close(client_fd);
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to allocate client TLS socket")));
    }

    client->fd = client_fd;
    client->ctx = NULL;  /* Client doesn't own the context */
    client->ssl = client_ssl;

    return lean_io_result_mk_ok(citadel_tls_socket_box(client));
}

/* Receive data over TLS */
LEAN_EXPORT lean_obj_res citadel_tls_socket_recv(
    b_lean_obj_arg sock_obj,
    uint32_t max_bytes,
    lean_obj_arg world
) {
    citadel_tls_socket_t *sock = citadel_tls_socket_unbox(sock_obj);

    uint8_t *buffer = malloc(max_bytes);
    if (!buffer) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to allocate buffer")));
    }

    int n = SSL_read(sock->ssl, buffer, max_bytes);
    if (n <= 0) {
        int err = SSL_get_error(sock->ssl, n);
        free(buffer);

        if (err == SSL_ERROR_ZERO_RETURN) {
            /* Clean shutdown - return empty array */
            return lean_io_result_mk_ok(lean_alloc_sarray(1, 0, 0));
        }

        if (err == SSL_ERROR_SYSCALL && errno == 0) {
            /* Connection closed unexpectedly - return empty array */
            return lean_io_result_mk_ok(lean_alloc_sarray(1, 0, 0));
        }

        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string(get_ssl_error_string())));
    }

    lean_obj_res arr = lean_alloc_sarray(1, n, n);
    memcpy(lean_sarray_cptr(arr), buffer, n);
    free(buffer);

    return lean_io_result_mk_ok(arr);
}

/* Send data over TLS */
LEAN_EXPORT lean_obj_res citadel_tls_socket_send(
    b_lean_obj_arg sock_obj,
    b_lean_obj_arg data,
    lean_obj_arg world
) {
    citadel_tls_socket_t *sock = citadel_tls_socket_unbox(sock_obj);

    size_t len = lean_sarray_size(data);
    const uint8_t *ptr = lean_sarray_cptr(data);

    size_t sent = 0;
    while (sent < len) {
        int n = SSL_write(sock->ssl, ptr + sent, len - sent);
        if (n <= 0) {
            int err = SSL_get_error(sock->ssl, n);
            char errbuf[256];
            snprintf(errbuf, sizeof(errbuf), "TLS write failed: %s (SSL error %d)",
                     get_ssl_error_string(), err);
            return lean_io_result_mk_error(lean_mk_io_user_error(
                lean_mk_string(errbuf)));
        }
        sent += n;
    }

    return lean_io_result_mk_ok(lean_box(0));
}

/* Close TLS socket */
LEAN_EXPORT lean_obj_res citadel_tls_socket_close(
    lean_obj_arg sock_obj,
    lean_obj_arg world
) {
    citadel_tls_socket_t *sock = citadel_tls_socket_unbox(sock_obj);

    if (sock->ssl) {
        SSL_shutdown(sock->ssl);
        SSL_free(sock->ssl);
        sock->ssl = NULL;
    }

    if (sock->fd >= 0) {
        close(sock->fd);
        sock->fd = -1;
    }

    lean_dec_ref(sock_obj);
    return lean_io_result_mk_ok(lean_box(0));
}

/* Set TLS socket recv/send timeouts in seconds */
LEAN_EXPORT lean_obj_res citadel_tls_socket_set_timeout(
    b_lean_obj_arg sock_obj,
    uint32_t timeout_secs,
    lean_obj_arg world
) {
    citadel_tls_socket_t *sock = citadel_tls_socket_unbox(sock_obj);

    struct timeval timeout;
    timeout.tv_sec = timeout_secs;
    timeout.tv_usec = 0;
    setsockopt(sock->fd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));
    setsockopt(sock->fd, SOL_SOCKET, SO_SNDTIMEO, &timeout, sizeof(timeout));

    return lean_io_result_mk_ok(lean_box(0));
}
