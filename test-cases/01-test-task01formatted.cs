class Hello
{
	void main ()
	{
		test(2 + 3);
		print('a');
		print(true && true);
		print(true && false);
		print(true || false);
		print(false || false);
	}
	void test (int x)
	{
		x = 4;
		{
			print(x++);
			print(++x);
		}
	}
}
