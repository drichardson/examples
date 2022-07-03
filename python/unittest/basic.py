import unittest
import unittest.mock
import p1.mod1

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

class MyClass:
    def foo(self):
        print("foo called")
        return 5

class MyException(Exception):
    pass


def raise_it():
    raise MyException("always throw exception")

def always_throw_MyClass():
    return unittest.mock.Mock(spec=MyClass, side_effect=raise_it)

class TestMyClass(unittest.TestCase):

    def test_myclass(self):
        c = MyClass()
        self.assertEqual(c.foo(), 5)

    def test_myclass_mock(self):
        c = MyClass()
        c.foo = unittest.mock.MagicMock(return_value=6)
        self.assertEqual(c.foo(), 6)

    def test_patch_mock(self):
        with unittest.mock.patch.object(MyClass, 'foo', return_value=7) as mock:
            c = MyClass()
            self.assertEquals(c.foo(), 7)
            mock.assert_called_once()
            mock.assert_called()
            mock.assert_called_with()


    def test_patch_mock(self):
        with unittest.mock.patch.object(MyClass, 'foo', return_value=7) as mock:
            c = MyClass()
            self.assertEqual(c.foo(), 7)
            mock.side_effect = raise_it
            with self.assertRaises(MyException):
                c.foo()

    def test_no_patch_decorator(self):
        self.assertTrue(p1.mod1.foo())
        
    @unittest.mock.patch('p1.mod1.foo')
    def test_patch_decorator1(self, mock : unittest.mock.MagicMock):
        mock.return_value = False
        self.assertFalse(p1.mod1.foo())

        mock.return_value = True
        self.assertTrue(p1.mod1.foo())

    @unittest.mock.patch('p1.mod1.foo', side_effect=raise_it)
    def test_patch_decorator1(self, mock : unittest.mock.MagicMock):
        with self.assertRaises(MyException):
            p1.mod1.foo()
        mock.assert_called_with()

    @unittest.mock.patch.object(MyClass, 'foo')
    def test_patch_object_2_arg(self, mock : unittest.mock.MagicMock):
        print(f"mock is {type(mock)}")
        c = MyClass()
        c.foo()
        mock.assert_called_with()
        c.foo(1)
        mock.assert_called_with(1)
        c.foo(2)
        mock.assert_called_with(2)
        
    @unittest.mock.patch.object(MyClass, 'foo', lambda self: 77)
    def test_patch_object_3_arg(self):
        c = MyClass()
        self.assertEqual(c.foo(), 77)


if __name__ == '__main__':
    print("calling unittest.main()")
    unittest.main()
