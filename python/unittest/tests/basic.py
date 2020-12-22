import unittest

class TestStringMethods(unittest.TestCase):

    def setUp(self):
        print("setUp()")

    def tearDown(self):
        print("tearDown()")

    def test_upper(self):
        print("test_upper()")
        self.assertEqual('foo'.upper(), 'FOO')

    def test_isupper(self):
        print("test_isupper()")
        self.assertTrue('FOO'.isupper())
        self.assertFalse('Foo'.isupper())

    def test_split(self):
        print("test_split()")
        s = 'hello world'
        self.assertEqual(s.split(), ['hello', 'world'])
        # check that s.split fails when the separator is not a string
        with self.assertRaises(TypeError):
            s.split(2)

if __name__ == '__main__':
    print("calling unittest.main()")
    unittest.main()
