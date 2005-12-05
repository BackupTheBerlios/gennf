file_list="$file_list"' unix.o'
mod_list="$mod_list"' unix'
make clisp-module CC="${CC}" CFLAGS="${CFLAGS}" INCLUDES="$absolute_linkkitdir"
NEW_FILES="$file_list wrap.o"
NEW_LIBS="$file_list wrap.o"
NEW_MODULES="$mod_list"
TO_LOAD='unix'
