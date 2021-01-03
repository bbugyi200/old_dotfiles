#include <fstream>
#include <iostream>
#include <string>
#include <vector>

//#define MY_DEBUG 1

struct term //each term of a polynomial
{
    term() = default;

    term(int power_x_, int power_y_, int coefficient_) : _power_x(power_x_), _power_y(power_y_), _coefficient(coefficient_)
    {}

    ~term() = default;
    term(term const&) = default;
    term& operator=(term const&) = default;

    term multiply(term const& other_) const
    {
        return term {_power_x + other_._power_x, _power_y + other_._power_y, _coefficient*other_._coefficient};
    }

    bool valid() const { return _coefficient; } 

    void reset() { _power_x = 0; _power_y = 0; _coefficient = 0; }

    bool can_merge(term const& other_) const
    {
        return (_power_x == other_._power_x && _power_y == other_._power_y);
    }

    term& merge(term const& other_)
    {
        _coefficient += other_._coefficient;

        return *this;
    }

    int _power_x{0}; 
    int _power_y{0};
    int _coefficient{0};
};

std::ostream& operator<<(std::ostream& ostr_, term const& t_) //output operator
{
    ostr_ << "[" << t_._coefficient << "," << t_._power_x << "," << t_._power_y << "]";

    return ostr_;
}

//polynomial - a vector of term objects
using polynomial=std::vector<term>;

static void read_poly(std::string const& line_, const int linecount_, polynomial& terms_) //parse a line from input and create a polynomial
{
    #ifdef MY_DEBUG
    std::cout << "parsing polynomial expression: " << line_ << std::endl;
    #endif

    term thisterm;
    bool scope_coefficient = false;
    bool scope_x = false;
    bool scope_y = false;
    std::string power_x;
    std::string power_y;
    std::string coefficient;
    for (const char c : line_)
    {
        switch (c)
        {
            case '+':
            case '-':
            case '$':
                if (thisterm.valid()) //save the previous term
                {
                    thisterm._power_x *= !power_x.empty() ? std::stoi(power_x) : 1;
                    thisterm._power_y *= !power_y.empty() ? std::stoi(power_y) : 1;
                    thisterm._coefficient *= !coefficient.empty() ? std::stoi(coefficient) : 1;
                    #ifdef MY_DEBUG
                    std::cout << thisterm;
                    #endif
                    terms_.emplace_back(thisterm); 
                    thisterm.reset();
                }
                coefficient = "";
                power_x = "";
                power_y = "";
                scope_coefficient = true;
                scope_x = false;
                scope_y = false;
                if (c == '+')
                    thisterm._coefficient = +1;
                else if (c == '-')
                    thisterm._coefficient = -1;
                break;
            case 'x':
                scope_coefficient = false;
                scope_x = true;
                scope_y = false;
                thisterm._power_x = 1;
                break;
            case 'y':
                scope_coefficient = false;
                scope_x = false;
                scope_y = true;
                thisterm._power_y = 1;
                break;
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                if (scope_coefficient)
                    coefficient += c;
                else if (scope_x)
                    power_x += c;
                else if (scope_y)
                    power_y += c;
                else
                    throw std::runtime_error("logic problem somewhere");
                break;
            default : throw std::runtime_error("invalid character '" + std::string {c} + "' at line " + std::to_string(linecount_));
        }
    }
}

//
//multiply two polynomials
//merge similar terms
//store result in 3rd polynomial
//
void mult_poly(polynomial const& poly1_, polynomial const& poly2_, polynomial& result_)
{
    polynomial tmp;
    for (auto&& t1 : poly1_)
    {
        for (auto&& t2 : poly2_)
        {
            tmp.emplace_back(t1.multiply(t2));
        }
    }

    //sort here
    std::sort(tmp.begin(), tmp.end(), [](term const& a, term const& b) { 
        if (a._power_x == b._power_x)
            return (a._power_y < b._power_y);
        else
            return !(a._power_x < b._power_x);
    });

    //merge similar terms and put the result into final destination
    for (auto&& t : tmp)
    {
        if (result_.size())
        {
            //check if we can merge this term
            auto& last = result_.back();
            if (last.can_merge(t))
                last.merge(t);
            else
                result_.emplace_back(t);
        }
        else
            result_.emplace_back(t);
    }
}

//display the polynomial
void disp_poly(polynomial const& poly)
{
    std::cout << "---------------------\n";
    for (auto&& t : poly)
    {
        std::cout << t << ",";
    }
    std::cout << "\n";
    std::cout << "---------------------\n";
}

int main(int argc_, char* argv_[])
{
    if (argc_ !=2 )
    {
        std::cerr << "usage: " << argv_[0] << " <input>" << std::endl;
        return -1;
    }

    std::ifstream ifstrm(argv_[1]);
    if (!ifstrm)
    {
        std::cerr << "error: failed to read input file " << argv_[1] << std::endl;
        return -1;
    }
    
    int linecount = 0;
    polynomial poly1;
    polynomial poly2;
    for (;;)
    {
        std::string next;
        std::getline(ifstrm, next);
        if (ifstrm.eof())
            break;
        if (next[0] == '#')
            break;

        ++linecount;
      
        if (linecount % 2) //first of a pair
        {
            if (next[0] == '+' || next[0] == '-')
                read_poly(next + "$", linecount, poly1);
            else
                read_poly("+" + next + "$", linecount, poly1);
        }
        else //last of a pair
        {
            polynomial result;
            if (next[0] == '+' || next[0] == '-')
                read_poly(next + "$", linecount, poly2);
            else 
                read_poly("+" + next + "$", linecount, poly2);
            mult_poly(poly1, poly2, result);
            disp_poly(result);
            poly1.clear();
            poly2.clear();
        }
    }

    return 0;
}
