#!/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc underscore.js 

print("collections.js example");

//
// Container functions
// 

// each
_.each([1, 2, 3], function(element, index, list) {
    print("Element: " + element + ", Index: " + index + ", List: " + list)
});

// map
var input = [1, 2, 3, 11, 12, 13]
var result = _.map(input, function(value, key, list) { return value * value; })
print("Mapped " + input + " to " + result)


// reduce
result = _.reduce(input, function(memo, num) { return memo + num; }, 0)
print("Input " + input + " reduced to " + result)

// filter
result = _.filter(input, function(num) { return num % 2 == 0; })
print("Input " + input + " filtered to " + result)


//
// Function functions
//
var func = function(greeting) { return greeting + ": " + this.name }
func = _.bind(func, {name : 'moe'}, 'hi')
print("bind: func() = " + func())

