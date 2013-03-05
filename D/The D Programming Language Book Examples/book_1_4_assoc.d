import std.stdio, std.string, std.array, std.conv;

void main()
{
	uint[string] dictionary;
	foreach (line; stdin.byLine())
	{
		foreach(word; splitter(strip(line)))
		{
			string sWord = to!string(word);
			if ( sWord in dictionary ) continue;
			auto newID = cast(uint)dictionary.length;
			dictionary[sWord] = newID;
			writeln(newID, '\t', sWord);
		}
	}
}
