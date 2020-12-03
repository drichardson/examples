import std.algorithm, std.conv, std.ctype, std.regex, std.range, std.stdio, std.string;

struct PersonaData {
	uint totalWordCount;
	uint[string] wordCount;
}

void main() {
	// Accumulates information about dramatis personae
	PersonaData[string] info;
	// Fill info
	string currentParagraph;
	foreach(line; stdin.byLine()){
		if(line.startsWith(" ") && line.length > 4 && isalpha(line[4])) {
			// Persona is continuing a line
			currentParagraph ~= line[3..$];
		} else if (line.startsWith(" ") && line.length > 2 && isalpha(line[2])) {
			// Persona just started speaking
			addParagraph(currentParagraph, info);
			currentParagraph = to!string(line[2..$]);
		}
	}

	// Done, now print collected information
	printResults(info);
}