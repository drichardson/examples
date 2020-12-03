console.log('__tests__/sum.js');

const sum = require('../sum');

test('10+20=30', () => {
    expect(sum(10,20)).toBe(30);
});

test('10-20=-10 (fails on purpose)', () => {
    expect(sum(10,-20)).toBe(10);
});


