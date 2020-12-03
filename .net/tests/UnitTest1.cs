using System;
using Xunit;
using logic;

namespace tests
{
    public class UnitTest1
    {
        [Fact]
        public void Test1()
        {
            var expectedMessage = "Hello Doug!";
            var actualMessage = HelloWorld.GetMessage("Doug");
            Assert.Equal(expectedMessage, actualMessage);
        }
    }
}
