out_dir=build
common_objs=addressbook.pb.o
list_objs=$(common_objs) list.o
list_objs_out=$(addprefix $(out_dir)/,$(list_objs))
write_objs=$(common_objs) write.o
write_objs_out=$(addprefix $(out_dir)/,$(write_objs))

protobuf_prefix=/opt/local
protobuf_cc_files=$(out_dir)/addressbook.pb.cc

CXX=clang++
CXXFLAGS=-Wall -Werror -g -Ibuild -I$(protobuf_prefix)/include

LD=$(CXX)
LIBS=-lprotobuf
LDFLAGS=-L/opt/local/lib $(LIBS)

all: $(out_dir)/list $(out_dir)/write

$(out_dir)/list: $(out_dir) $(list_objs_out)
	$(LD) $(LDFLAGS) -o $@ $(list_objs_out)

$(out_dir)/write: $(out_dir) $(write_objs_out)
	$(LD) $(LDFLAGS) -o $@ $(write_objs_out)

$(out_dir):
	mkdir $(out_dir)

$(out_dir)/%.pb.o: $(out_dir)/%.pb.cc
	$(CXX) -c $(CXXFLAGS) -o $@ $<

$(out_dir)/%.o: %.cc
	$(CXX) -c $(CXXFLAGS) $< -o $@

$(out_dir)/%.pb.cc: %.proto
	protoc -I=. --cpp_out=$(out_dir) $<

clean:
	$(RM) -r $(out_dir)
