import std.stdio, std.string, std.conv, std.algorithm;

void main() {
	// Compute counts
	uint[string] freqs;
	foreach(line; stdin.byLine()){
		foreach(word; split(strip(line))){
			++freqs[to!string(word)];
		}
	}

	auto words = freqs.keys;
	sort!((a,b) { return freqs[a] > freqs[b]; })(words);

	// Print counts
	foreach(word; words){
		writefln("%6u\t%s", freqs[word], word);
	}
}
