CXX = clang++
DEBUG = -DDEBUG
CXXFLAGS = -std=c++2b \
	-Wall -Weverything \
	-Wno-c++98-compat \
	-Wno-c++98-compat-pedantic \
	-Wno-shadow-field-in-constructor \
	-Wno-padded \
	-Wno-disabled-macro-expansion
LDFLAGS = -licuuc

TARGET = syscraws

SRC = $(wildcard *.cpp)
OBJ = $(SRC:.cpp=.o)
DEP = $(SRC:.cpp=.d)

$(TARGET): $(OBJ)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $(OBJ)

%.o: %.cpp
	$(CXX) -c -MMD $(CXXFLAGS) $(DEBUG) $<

-include $(DEP)

.PHONY: clean
clean:
	$(RM) $(OBJ) $(DEP) $(TARGET)