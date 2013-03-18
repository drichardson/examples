import std.stdio, std.string, std.conv;

void main() {
	// Compute counts
	uint[string] freqs;
	foreach(line; stdin.byLine()){
		foreach(word; split(strip(line))){
			++freqs[to!string(word)];
		}
	}

	// Print counts
	foreach(key, value; freqs){
		writefln("%6u\t%s", value, key);
	}
}