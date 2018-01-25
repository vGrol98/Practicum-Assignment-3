class Hello {
    void main() {
        test(2 + 3);

        print('a');

        print(true && true);
        print(true && false);
        print(true || error());
        print(false || false);
    }

    bool error(){
        print(666);
        return false;
    }

    void test(int x){
        x=4;
        {
        print(x++);
        print(++x);
        }
    }
}
