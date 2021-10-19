#include <cstring>
#include <iostream>

using std::cout;

class Base1
{
public:
	virtual void foo() { cout << "Base1::foo\n"; };
};

class Base2
{
public:
	virtual void foo() { cout << "Base2::foo\n"; }
	virtual void bar() { cout << "Base2::bar\n"; }
};

class MultipleAmbiguous : public Base1, public Base2
{
public:
	void foo() override { cout << "Multiple::foo\n"; }
	void bar() override { cout << "Multiple::bar\n"; }

	~MultipleAmbiguous() { cout << "~MultipleAmbiguous\n"; }
};

class X
{
public:
	X() : s(buf)
	{
		std::strcpy(buf, "X");
		cout << "X(" << this << ")\n";
	}

	~X() { cout << "~X(" << this << ") - s=" << (s ? s : "NULL") << "\n"; }

	X(const X &other)
	{
		std::memcpy(buf, other.buf, sizeof(buf) / sizeof(buf[0]));
		std::strcat(buf, "COPY");
		s = buf;
		cout << "X copyctor(this=" << this << " other=" << &other
		     << " s=" << s << ")\n";
	}

	X &operator=(const X &other)
	{
		std::memcpy(buf, other.buf, sizeof(buf) / sizeof(buf[0]));
		std::strcat(buf, "ASSIGN");
		s = buf;
		cout << "X operator=(this=" << this << ", other=" << &other
		     << ")\n";
		return *this;
	}

	char buf[20];
	char *s;
};

class X1 : public X
{
public:
	X1()
	{
		std::strcpy(buf, "X1");
		cout << "X1(" << this << ")\n";
	}

	~X1() { cout << "~X1(" << this << ")\n"; }
};

class X2 : public X
{
public:
	X2()
	{
		char *ss = new char[40];
		std::strncpy(ss, "X2", 10);
		ss[9] = 0;
		s = ss;
		cout << "X2(" << this << "\n";
	}

	~X2()
	{
		cout << "~X2(" << this << ")\n";
		delete[] s;
		s = nullptr; // since X is going to log this
	}
};

int main()
{
	{
		cout << "Non-virtual function call =========\n";
		MultipleAmbiguous m;
		m.foo();
		m.bar();

		cout << "Virtual thourgh Base1* =========\n";
		Base1 *b1 = &m;
		b1->foo();

		cout << "Non-Virtual thourgh Base1* =========\n";
		b1->Base1::foo();

		cout << "Virtual thourgh Base2* =========\n";
		Base2 *b2 = &m;
		b2->foo();
		b2->bar();

		cout << "Non-Virtual thourgh Base2* =========\n";
		b2->Base2::foo();
		b2->Base2::bar();

		cout << "Virtual thourgh Base2& =========\n";
		Base2 &b2ref = m;
		b2ref.foo();
		b2ref.bar();

		cout << "Non-virtual through Base2& ==========\n";
		b2ref.Base2::foo();
		b2ref.Base2::bar();
	}

	cout << "1: X x\n";
	{
		X x;
	}
	cout << "2: X1 x1\n";
	{
		X1 x1;
	}
	cout << "3: X2 x2\n";
	{
		X2 x2;
	}
	cout << "4: X x = X1()\n";
	{
		X x = X1();
	}
	cout << "5: X x = X2()\n";
	{
		X x = X2();
	}
	cout << "6: X x; x = X2()\n";
	{
		X x;
		x = X2();
	}
	cout << "7\n";

	return 0;
}
