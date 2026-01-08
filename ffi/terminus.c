// Terminus FFI - Terminal operations via termios

#include <lean/lean.h>
#include <termios.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <poll.h>
#include <fcntl.h>
#include <errno.h>

// Store original terminal settings for restoration
static struct termios original_termios;
static int raw_mode_enabled = 0;
#define TERMINUS_UNREAD_BUF_SIZE 64
static unsigned char unread_buf[TERMINUS_UNREAD_BUF_SIZE];
static int unread_len = 0;

// Enable raw mode - disable canonical mode, echo, and signals
LEAN_EXPORT lean_obj_res terminus_enable_raw_mode(lean_obj_arg world) {
    if (raw_mode_enabled) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    if (tcgetattr(STDIN_FILENO, &original_termios) == -1) {
        return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string("Failed to get terminal attributes")));
    }

    struct termios raw = original_termios;

    // Input flags: disable break signal, CR to NL, parity check, strip 8th bit, XON/XOFF
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);

    // Output flags: disable post-processing
    raw.c_oflag &= ~(OPOST);

    // Control flags: set 8-bit chars
    raw.c_cflag |= (CS8);

    // Local flags: disable echo, canonical mode, signals, extended input
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);

    // Control chars: non-blocking read (VMIN=0, VTIME=0 means return immediately)
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 0; // No timeout - return immediately

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) {
        return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string("Failed to set terminal to raw mode")));
    }

    raw_mode_enabled = 1;
    return lean_io_result_mk_ok(lean_box(0));
}

// Disable raw mode - restore original terminal settings
LEAN_EXPORT lean_obj_res terminus_disable_raw_mode(lean_obj_arg world) {
    if (!raw_mode_enabled) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &original_termios) == -1) {
        return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string("Failed to restore terminal settings")));
    }

    raw_mode_enabled = 0;
    return lean_io_result_mk_ok(lean_box(0));
}

// Get terminal size as (width, height)
LEAN_EXPORT lean_obj_res terminus_get_size(lean_obj_arg world) {
    struct winsize ws;

    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
        // Fallback to default size
        ws.ws_col = 80;
        ws.ws_row = 24;
    }

    lean_object* pair = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(pair, 0, lean_box(ws.ws_col));
    lean_ctor_set(pair, 1, lean_box(ws.ws_row));

    return lean_io_result_mk_ok(pair);
}

// Read a single byte from stdin, returns Option UInt8
// Returns none if no byte available (non-blocking)
LEAN_EXPORT lean_obj_res terminus_read_byte(lean_obj_arg world) {
    unsigned char c;
    if (unread_len > 0) {
        c = unread_buf[--unread_len];
        lean_object* some = lean_alloc_ctor(1, 1, 0);
        lean_ctor_set(some, 0, lean_box(c));
        return lean_io_result_mk_ok(some);
    }
    ssize_t nread = read(STDIN_FILENO, &c, 1);

    if (nread == 1) {
        // Some c
        lean_object* some = lean_alloc_ctor(1, 1, 0);
        lean_ctor_set(some, 0, lean_box(c));
        return lean_io_result_mk_ok(some);
    } else {
        // None
        return lean_io_result_mk_ok(lean_box(0));
    }
}

// Read a single byte from stdin, blocking until input is available
LEAN_EXPORT lean_obj_res terminus_read_byte_blocking(lean_obj_arg world) {
    unsigned char c;
    if (unread_len > 0) {
        c = unread_buf[--unread_len];
        lean_object* some = lean_alloc_ctor(1, 1, 0);
        lean_ctor_set(some, 0, lean_box(c));
        return lean_io_result_mk_ok(some);
    }

    for (;;) {
        struct pollfd pfd;
        pfd.fd = STDIN_FILENO;
        pfd.events = POLLIN;
        pfd.revents = 0;

        int ready = poll(&pfd, 1, -1);
        if (ready == -1) {
            if (errno == EINTR) {
                continue;
            }
            return lean_io_result_mk_error(
                lean_mk_io_user_error(lean_mk_string("Failed to poll for input")));
        }

        if (pfd.revents & POLLIN) {
            ssize_t nread = read(STDIN_FILENO, &c, 1);
            if (nread == 1) {
                lean_object* some = lean_alloc_ctor(1, 1, 0);
                lean_ctor_set(some, 0, lean_box(c));
                return lean_io_result_mk_ok(some);
            }
            if (nread == 0) {
                return lean_io_result_mk_ok(lean_box(0));
            }
            if (errno == EINTR || errno == EAGAIN || errno == EWOULDBLOCK) {
                continue;
            }
            return lean_io_result_mk_error(
                lean_mk_io_user_error(lean_mk_string("Failed to read input")));
        }

        if (pfd.revents & POLLHUP) {
            return lean_io_result_mk_ok(lean_box(0));
        }

        if (pfd.revents & (POLLERR | POLLNVAL)) {
            return lean_io_result_mk_error(
                lean_mk_io_user_error(lean_mk_string("Input polling error")));
        }
    }
}

// Push a byte back onto the input stream
LEAN_EXPORT lean_obj_res terminus_unread_byte(b_lean_obj_arg byte, lean_obj_arg world) {
    if (unread_len < TERMINUS_UNREAD_BUF_SIZE) {
        unread_buf[unread_len++] = (unsigned char)lean_unbox(byte);
    }
    return lean_io_result_mk_ok(lean_box(0));
}

// Write a string to stdout
LEAN_EXPORT lean_obj_res terminus_write_stdout(b_lean_obj_arg str, lean_obj_arg world) {
    const char* s = lean_string_cstr(str);
    size_t len = lean_string_size(str) - 1; // Exclude null terminator

    ssize_t written = write(STDOUT_FILENO, s, len);
    if (written == -1) {
        return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string("Failed to write to stdout")));
    }

    return lean_io_result_mk_ok(lean_box(0));
}

// Flush stdout
LEAN_EXPORT lean_obj_res terminus_flush_stdout(lean_obj_arg world) {
    // STDOUT_FILENO is unbuffered at fd level, but ensure write completes
    if (fsync(STDOUT_FILENO) == -1 && errno != EINVAL && errno != ENOTSUP) {
        // fsync may fail on terminals, which is fine
    }
    return lean_io_result_mk_ok(lean_box(0));
}
