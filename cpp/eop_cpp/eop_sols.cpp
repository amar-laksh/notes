#include <eop.hpp>
#include <iostream>
using namespace std;
using namespace eop;
using namespace chrono;

int main(int argc, char** argv)
{
    enum week { Mon,
        Tue,
        Wed,
        Thur,
        Fri,
        Sat,
        Sun };

    if (argc != 1) {
        std::cout << argv[0] << " takes no arguments.\n";
        return 1;
    }
    std::vector<std::string> a = {"hello"};
    std::vector<std::string> b = {"hello"};
    Eop eop;
/*     std::cout << "a:= " << a << " [" << eop.type_name<decltype(a)>() << "]" << '\n'; */
    /* std::cout << "b:= " << b << " [" << eop.type_name<decltype(b)>() << "]" << '\n'; */
    std::cout << "type equality: " << ((a<typeEquality> b) ? "true" : "false") << '\n';
    // std::cout << "representational equality: " << (eop.RepEq(a,b) ? "true" : "false") << '\n';
    std::cout << "Type equality => representational equality: " << (eop.RepEqImpliesEq(a, b) ? "holds" : "doesn't hold") << '\n';
    std::cout << "Representational equality => Type equality: " << (eop.EqImpliesRepEq(a, b) ? "holds" : "doesn't hold") << '\n';
    eop.ValueTypeIs(a, b);
    return 0;
}
