ERL_CFLAGS ?= -finline-functions -Wall -fPIC -I "/usr/lib/erlang/erts-10.6.4/include" -I "/usr/lib/erlang/lib/erl_interface-3.13.1/include"
ERL_LDFLAGS ?= -L "/usr/lib/erlang/lib/erl_interface-3.13.1/lib" -lei

ERL_LDFLAGS =  -L$(ERL_EI_LIBDIR) -lei
FLTO_FLAG = -flto
CFLAGS =  -Ic_src/ -g -Wall $(FLTO_FLAG)  -O3
CXXFLAGS =  -Ic_src/ -g -Wall $(FLTO_FLAG)  -O3
LDFLAGS =  $(FLTO_FLAG) -lstdc++

all:: priv/jiffy.so
	@:

%.o: %.c
	$(CC) -c -o $@ $< $(CFLAGS) $(ERL_CFLAGS) $(DRV_CFLAGS) $(EXE_CFLAGS)

%.o: %.C
	$(CXX) -c -o $@ $< $(CXXFLAGS) $(ERL_CFLAGS) $(DRV_CFLAGS) $(EXE_CFLAGS)

%.o: %.cc
	$(CXX) -c -o $@ $< $(CXXFLAGS) $(ERL_CFLAGS) $(DRV_CFLAGS) $(EXE_CFLAGS)

%.o: %.cpp
	$(CXX) -c -o $@ $< $(CXXFLAGS) $(ERL_CFLAGS) $(DRV_CFLAGS) $(EXE_CFLAGS)

priv/jiffy.so: $(foreach ext,.c .C .cc .cpp,$(patsubst %$(ext),%.o,$(filter %$(ext),$(wildcard c_src/*.c c_src/*.cc c_src/double-conversion/*.cc))))
	$(CC) -o $@ $? $(LDFLAGS) $(ERL_LDFLAGS) $(DRV_LDFLAGS) $(EXE_LDFLAGS) -shared
