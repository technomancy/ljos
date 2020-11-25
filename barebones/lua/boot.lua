package.cpath = "/lib/?.so;".."/lib64/?.so;/?.so;"
package.cpath = package.cpath.."/lua/libs/shared/?.so"
package.path = "/?.lua;/lua/?.lua;/lib/?.so;/lua/libs/?.lua;/lua/libs/?/init.lua"

local ffi = require("ffi")

ffi.cdef[[

void sleep( unsigned int sec );

int dup2(int oldfd, int newfd);
int open(const char *pathname, int flags, int mode);

unsigned int read(int fd, void *buf, unsigned int count);
unsigned int write(int fd, const void *buf, unsigned int count);

int execvp(const char *file, char *const argv[]);

/*
long syscall(long number, ...);
dev_t makedev(int major, int minor);
int mknod(const char *path, mode_t mode, dev_t dev);
*/
]]

ffi.load("/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2", true)
ffi.load("/lib/x86_64-linux-gnu/libc.so.6", true)

-- TODO: this causes a kernel panic
-- package.preload.lfs = function return require("syscall.lfs") end

print("\027c") -- clear screen
print("loading Polywell...")

local modulename = "polywell.lib.fennel"
local fennel = require(modulename)
table.insert(package.loaders, fennel.make_searcher({correlate=true,
                                                    useMetadata=true,
                                                    moduleName = modulename}))

local editor = require("polywell")
require("config") -- user-level config

local term = require("polywell.lib.plterm")
local frontend = require("polywell.frontend")
local readkey = term.input()
while true do
   xpcall(function()
         editor.draw()
         local key = frontend.key(readkey)
         editor.handlers.textinput(key)
         editor.handlers.keypressed(key)
         editor.handlers.keyreleased(key)
          end, function(e)
         term.setsanemode()
         print(e, debug.traceback())
         frontend.quit()
   end)
end
