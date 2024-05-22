#include <iostream>
#include <array>
#include <vector>
#include <filesystem>
#include <fstream>
#include <algorithm>
#include <memory>
#include <limits>
#include <cmath>
#include <format>
#include <cstdlib>
#include <omp.h>
#include <chrono>
#include <thread>
#include <initializer_list>
#define PI 3.14159265358979323846

using std::cout, std::endl, std::fstream, std::ofstream, std::array, std::vector, std::string, std::find, std::tuple, std::make_tuple, std::pair, std::make_pair, std::atan, std::sin, std::cos, std::abs;
struct CountryData {
    double value;
    double x;
    double y;
};
typedef void(*jiggle_func)(vector<CountryData>&, const vector<vector<size_t>>&);
typedef void(*radius_func)(vector<CountryData>&, const vector<vector<size_t>>&, const vector<vector<double>>&, const size_t, const vector<string>&, const string&);
template<typename T>
struct Heuristic {
    vector<T> funcs;
    vector<bool> already_served;
    vector<size_t> last_found_improvement;
    size_t last_served{ ~0ULL };

    Heuristic(std::initializer_list<T> l) :funcs{ l }, already_served{}, last_found_improvement{} {
        for (int i = 0; i < funcs.size(); ++i) {
            already_served.push_back(false);
            last_found_improvement.push_back(0);
        }
    }
    T next() {
        int last_improvement{ std::numeric_limits<int>::max() };
        T ret{ nullptr };
        size_t index{ 0 };
        for (int i = 0; i < funcs.size(); ++i) {
            if (!already_served[i]) {
                if (last_found_improvement[i] < last_improvement) {
                    last_improvement = last_found_improvement[i];
                    ret = funcs[i];
                    index = i;
                }
            }
        }
        already_served[index] = true;
        last_served = index;
        return ret;
    }
    void next_iteration() {
        for (int i = 0; i < already_served.size(); ++i) {
            already_served[i] = false;
            last_found_improvement[i] += 1;
        }
    }
    void mark_found() {
        for (int i = 0; i < last_found_improvement.size(); ++i) {
            last_found_improvement[i] += 1;
        }
        last_found_improvement[last_served] = 0;
    }
    size_t size() const { return funcs.size(); }
};
inline size_t getNameIndex(const vector<string>& v, const string& name) {
    for (int i = 0; i < v.size(); ++i) {
        if (v[i] == name) return i;
    }
    return ~0ULL;
}
vector<tuple<vector<CountryData>, vector<string>, vector<vector<size_t>>, string>> readInputFiles() {
    vector<tuple<vector<CountryData>, vector<string>, vector<vector<size_t>>, string>> ret{};
    const array<string, 12> files{
        "Area_Afro-Eurasia.txt",
        "Area_Americas.txt",
        "Area_Asia.txt",
        "Area_Europe.txt",
        "CO2_Production_Afro-Eurasia.txt",
        "Deutschlands_Nachbarn.txt",
        "GNI_per_capita_Afro-Eurasia.txt",
        "Instant_Noodle_Consumption_Eurasia.txt",
        "Population_Afro-Eurasia.txt",
        "Population_Americas.txt",
        "Population_Density_Afro-Eurasia.txt",
        "Population_Density_Americas.txt"
    };
    for (const auto& filename : files) {
        fstream file{ "../../../../input_files/" + filename, std::ios::in };
        if (file.is_open()) {
            vector<CountryData> lines{};
            vector<string> names{};
            string name;
            double value, x, y;
            size_t id{ 0 };
            while (true) {
                file >> name;
                if (std::find(names.begin(), names.end(), name) != names.end()) break;
                names.push_back(name);
                file >> value >> x >> y;
                value = sqrt(value / PI);
                lines.push_back(CountryData{ value,x,y });
            }
            string neighbour;
            file >> neighbour;
            vector<vector<size_t>> neighbours{};
            neighbours.reserve(names.size());
            for (int i = 0; i < names.size(); ++i) {
                neighbours.push_back(vector<size_t>{});
            }
            neighbours[getNameIndex(names, name)] = vector<size_t>{ getNameIndex(names,neighbour) };
            while (file >> name) {
                file >> neighbour;
                const size_t nameInd{ getNameIndex(names,name) };
                const size_t neighbourInd{ getNameIndex(names,neighbour) };
                neighbours[nameInd].push_back(neighbourInd);
            }
            file.close();
            ret.push_back(make_tuple(lines, names, neighbours, filename));
        }
        else {
            ret.push_back(make_tuple(vector<CountryData>{}, vector<string>{}, vector<vector<size_t>>{}, filename));
        }
    }
    return ret;
}
void write_result(vector<CountryData>& countries, const vector<string>& names, const string& file_name) {
    const string& filename = file_name;
    fstream file{ "../../../../result_files/" + filename + ".out", std::ios::out};
    if (file.is_open()) {
        for (int j = 0; j < countries.size(); ++j) {
            file << std::format("{} {} {} {} {}", countries[j].x, countries[j].y, countries[j].value, names[j], j) << endl;
        }
    }
    else {
        cout << "could not save " << filename << endl;
    }
}
inline double get_angle(const double dx, const double dy) {
    double angle;
    const double steigungsWinkel{ atan(dy / dx) };
    if (dx < 0) {
        angle = PI + steigungsWinkel;
    }
    else if (dx > 0) {
        if (steigungsWinkel >= 0) {
            angle = steigungsWinkel;
        }
        else {
            angle = 2 * PI + steigungsWinkel;
        }
    }
    else {
        angle = PI + (1 - 2 * (dy > 0)) * PI / 2;
    }
    return angle;
}
inline double angle_delta(const double angle1, const double angle2) {
    double d_angle{ angle2-angle1 + 4 * PI };
    while (d_angle > 0) {
        d_angle -= 2 * PI;
        if (d_angle < 0) {
            if (abs(d_angle + 2 * PI) > abs(d_angle)) {
                break;
            }
            d_angle += 2 * PI;
            break;
        }
    }
    return d_angle;
}
inline double get_relative_distance(const double dx, const double dy, const double r1, const double r2) {
    const double ideal_distance{ r1 + r2 };
    const double distance{ std::sqrt(dx * dx + dy * dy) };
    return 100 * (distance - ideal_distance) / ideal_distance;
}
inline double get_relative_overlap(const double dx, const double dy, const double r1, const double r2) {
    const double ideal_distance{ r1 + r2 };
    const double distance{ std::sqrt(dx * dx + dy * dy) };
    return 100 * (ideal_distance - distance) / ideal_distance;
}
inline double get_relative_angle_delta(const double dx, const double dy, const double initial_angle) {
    const double angle{ get_angle(dx,dy) };
    const double angle_delta = abs(initial_angle - angle);
    return 100 * std::min(angle_delta, 2 * PI - angle_delta) / PI;
}
vector<vector<double>> calculateInitialAngles(const vector<CountryData>&countries, const vector<vector<size_t>>&neighbours) {
    vector<vector<double>> ret{};
    for (int i = 0; i < countries.size();++i) {
        vector<double> arr{};
        for (int j = 0; j < neighbours[i].size(); ++j) {
            const double dx = (countries[neighbours[i][j]].x - countries[i].x);
            const double dy = (countries[neighbours[i][j]].y - countries[i].y);
            arr.push_back(get_angle(dx,dy));
        }
        ret.push_back(arr);
    }
    return ret;
}
constexpr double signum(double x) {
    return (0 < x) - (x < 0);
}
constexpr double radToDeg(double x) {
    return x * 360 / (2 * PI);
}
void normalizeRadiiMin(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours, const vector<vector<double>>& initialAngles, const size_t k, const vector<string>& names, const string& file_name) {
    double min{ std::numeric_limits<double>::max() };
    for (int i = 0; i < countries.size(); ++i) {
        if (countries[i].value) {
            min = std::min(min, countries[i].value);
        }
    }
    for (int i = 0; i < countries.size(); ++i) {
        countries[i].value = countries[i].value / min;
    }
}
void normalizeRadiiMax(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours, const vector<vector<double>>& initialAngles, const size_t k, const vector<string>& names, const string& file_name) {
    double max{ std::numeric_limits<double>::min() };
    for (int i = 0; i < countries.size(); ++i) {
        max = std::max(max, countries[i].value);
    }
    for (int i = 0; i < countries.size(); ++i) {
        countries[i].value = countries[i].value / max;
    }
}
void normalizeRadii(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours, const vector<vector<double>>& initialAngles, const size_t k, const vector<string>& names, const string& file_name) {
    double max{ std::numeric_limits<double>::min() };
    double min_dist{ std::numeric_limits<double>::max() };
    for (int i = 0; i < countries.size(); ++i) {
        max = std::max(max, countries[i].value);
        for (int j = i + 1; j < countries.size(); ++j) {
            const double dx{ countries[i].x - countries[j].x };
            const double dy{ countries[i].y - countries[j].y };
            const double dist{ std::sqrt(dx * dx + dy * dy) };
            if (dist) {
                min_dist = std::min(min_dist, dist);
            }
        }
    }
    const double factor = 0.5 * min_dist / max;
    for (int i = 0; i < countries.size(); ++i) {
        countries[i].value = countries[i].value * factor;
    }
}
void normalizeRadiiBoth(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours, const vector<vector<double>>& initialAngles, const size_t k, const vector<string>& names, const string& file_name) {
    normalizeRadiiMin(countries, neighbours, initialAngles, k, names, file_name);
    normalizeRadiiMax(countries, neighbours, initialAngles, k, names, file_name);
}
void normalizeRadiiNone(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours, const vector<vector<double>>& initialAngles, const size_t k, const vector<string>& names, const string& file_name){}
double get_attraction(CountryData& c1, CountryData& c2) {
    const double dx{ c2.x - c1.x };
    const double dy{ c2.y - c1.y };
    return get_relative_distance(dx, dy, c1.value, c2.value);
}
double get_repulsion(CountryData& c1, CountryData& c2) {
    const double dx{ c2.x - c1.x };
    const double dy{ c2.y - c1.y };
    return get_relative_overlap(dx, dy, c1.value, c2.value);
}
void apply_forces(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours) {
    double max_radius{ std::numeric_limits<double>::min() };
    double min_radius{ std::numeric_limits<double>::max() };
    for (int i = 0; i < countries.size(); ++i) {
        max_radius = std::max(max_radius, countries[i].value);
        if (countries[i].value > 0) {
            min_radius = std::min(min_radius, countries[i].value);
        }
    }
    const double steps{ -0.1 };
    vector<pair<double, double>> forces{};
    for (int i = 0; i < countries.size(); ++i) {
        double f_x{ 0 };
        double f_y{ 0 };
        for (int j = 0; j < neighbours[i].size(); ++j) {
            const double angle{ get_angle(countries[neighbours[i][j]].x - countries[i].x,countries[neighbours[i][j]].y - countries[i].y) };
            f_x += std::cos(angle) * steps;
            f_y += std::sin(angle) * steps;
        }
        for (int j = 0; j < countries.size(); ++j) {
            if (i == j) continue;
            const double angle{ get_angle(countries[j].x - countries[i].x,countries[j].y - countries[i].y) };
            f_x += std::cos(angle) * steps;
            f_y += std::sin(angle) * steps;
        }
        forces.push_back({ f_x, f_y });
    }
    for (int i = 0; i < countries.size(); ++i) {
        countries[i].x += forces[i].first;
        countries[i].y += forces[i].second;
    }
}
void apply_forces_negative(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours) {
    double max_radius{ std::numeric_limits<double>::min() };
    double min_radius{ std::numeric_limits<double>::max() };
    for (int i = 0; i < countries.size(); ++i) {
        max_radius = std::max(max_radius, countries[i].value);
        if (countries[i].value > 0) {
            min_radius = std::min(min_radius, countries[i].value);
        }
    }
    const double steps{ -0.1 };
    vector<pair<double, double>> forces{};
    for (int i = 0; i < countries.size(); ++i) {
        double f_x{ 0 };
        double f_y{ 0 };
        for (int j = 0; j < neighbours[i].size(); ++j) {
            const double angle{ get_angle(countries[neighbours[i][j]].x - countries[i].x,countries[neighbours[i][j]].y - countries[i].y) };
            f_x += std::cos(angle) * steps;
            f_y += std::sin(angle) * steps;
        }
        for (int j = 0; j < countries.size(); ++j) {
            if (i == j) continue;
            const double angle{ get_angle(countries[j].x - countries[i].x,countries[j].y - countries[i].y) };
            f_x += std::cos(angle) * steps;
            f_y += std::sin(angle) * steps;
        }
        forces.push_back({ f_x, f_y });
    }
    for (int i = 0; i < countries.size(); ++i) {
        countries[i].x -= forces[i].first;
        countries[i].y -= forces[i].second;
    }
}
void apply_forces_old(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours) {
    double max_radius{ std::numeric_limits<double>::min() };
    double min_radius{ std::numeric_limits<double>::max() };
    for (int i = 0; i < countries.size(); ++i) {
        max_radius = std::max(max_radius, countries[i].value);
        if (countries[i].value > 0) {
            min_radius = std::min(min_radius, countries[i].value);
        }
    }
    const double step_size{ 0.00075 * min_radius / max_radius };
    vector<pair<double, double>> forces{};
    for (int i = 0; i < countries.size(); ++i) {
        double f_x{ 0 };
        double f_y{ 0 };
        for (int j = 0; j < neighbours[i].size(); ++j) {
            const double force{ -step_size * get_attraction(countries[i], countries[neighbours[i][j]]) };
            const double angle{ get_angle(countries[neighbours[i][j]].x - countries[i].x,countries[neighbours[i][j]].y - countries[i].y) };
            f_x += std::cos(angle) * force;
            f_y += std::sin(angle) * force;
        }
        for (int j = 0; j < countries.size(); ++j) {
            if (i == j) continue;
            const double force{ -2 * step_size * get_repulsion(countries[i], countries[j]) };
            const double angle{ get_angle(countries[j].x - countries[i].x,countries[j].y - countries[i].y) };
            f_x += std::cos(angle) * force;
            f_y += std::sin(angle) * force;
        }
        forces.push_back({ f_x, f_y });
    }
    for (int i = 0; i < countries.size(); ++i) {
        countries[i].x += forces[i].first;
        countries[i].y += forces[i].second;
    }
}
void apply_forces_old_negative(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours) {
    double max_radius{ std::numeric_limits<double>::min() };
    double min_radius{ std::numeric_limits<double>::max() };
    for (int i = 0; i < countries.size(); ++i) {
        max_radius = std::max(max_radius, countries[i].value);
        if (countries[i].value > 0) {
            min_radius = std::min(min_radius, countries[i].value);
        }
    }
    const double step_size{ 0.00075 * min_radius / max_radius };
    vector<pair<double, double>> forces{};
    for (int i = 0; i < countries.size(); ++i) {
        double f_x{ 0 };
        double f_y{ 0 };
        for (int j = 0; j < neighbours[i].size(); ++j) {
            const double force{ -step_size * get_attraction(countries[i], countries[neighbours[i][j]]) };
            const double angle{ get_angle(countries[neighbours[i][j]].x - countries[i].x,countries[neighbours[i][j]].y - countries[i].y) };
            f_x += std::cos(angle) * force;
            f_y += std::sin(angle) * force;
        }
        for (int j = 0; j < countries.size(); ++j) {
            if (i == j) continue;
            const double force{ -2 * step_size * get_repulsion(countries[i], countries[j]) };
            const double angle{ get_angle(countries[j].x - countries[i].x,countries[j].y - countries[i].y) };
            f_x += std::cos(angle) * force;
            f_y += std::sin(angle) * force;
        }
        forces.push_back({ f_x, f_y });
    }
    for (int i = 0; i < countries.size(); ++i) {
        countries[i].x -= forces[i].first;
        countries[i].y -= forces[i].second;
    }
}
void apply_twist(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours) {
    double x{ 0 };
    double y{ 0 };
    for (int i = 0; i < countries.size(); ++i) {
        x += countries[i].x;
        y += countries[i].y;
    }
    x = x / countries.size();
    y = y / countries.size();
    double max_distance{ std::numeric_limits<double>::min() };
    for (int i = 0; i < countries.size(); ++i) {
        const double dx{ countries[i].x - x };
        const double dy{ countries[i].y - y };
        const double r = std::sqrt(dx * dx + dy * dy);
        max_distance = std::max(max_distance, r);
    }
    const double step_size{ 2 * PI * 0.01 };
    for (int i = 0; i < countries.size(); ++i) {
        const double dx{ countries[i].x - x };
        const double dy{ countries[i].y - y };
        const double r = std::sqrt(dx * dx + dy * dy);
        const double angle{ get_angle(dx,dy) };
        countries[i].x = x + r * std::cos(angle + step_size * max_distance / r);
        countries[i].y = y + r * std::sin(angle + step_size * max_distance / r);
    }
}
void apply_twist_negative(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours) {
    double x{ 0 };
    double y{ 0 };
    for (int i = 0; i < countries.size(); ++i) {
        x += countries[i].x;
        y += countries[i].y;
    }
    x = x / countries.size();
    y = y / countries.size();
    const double step_size{ 2 * PI * 0.0001 };
    for (int i = 0; i < countries.size(); ++i) {
        const double dx{ countries[i].x - x };
        const double dy{ countries[i].y - y };
        const double r = std::sqrt(dx * dx + dy * dy);
        const double angle{ get_angle(dx,dy) };
        countries[i].x = x + r * std::cos(angle - step_size);
        countries[i].y = y + r * std::sin(angle - step_size);
    }
}
void apply_twist_and_stretch(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours) {
    double x{ 0 };
    double y{ 0 };
    for (int i = 0; i < countries.size(); ++i) {
        x += countries[i].x;
        y += countries[i].y;
    }
    x = x / countries.size();
    y = y / countries.size();
    double max_distance{ std::numeric_limits<double>::min() };
    for (int i = 0; i < countries.size(); ++i) {
        const double dx{ countries[i].x - x };
        const double dy{ countries[i].y - y };
        const double r = std::sqrt(dx * dx + dy * dy);
        max_distance = std::max(max_distance, r);
    }
    const double step_size{ 2 * PI * 0.01 };
    for (int i = 0; i < countries.size(); ++i) {
        const double dx{ countries[i].x - x };
        const double dy{ countries[i].y - y };
        const double r = std::sqrt(dx * dx + dy * dy);
        const double angle{ get_angle(dx,dy) };
        countries[i].x = x + 1.05 * r * std::cos(angle + step_size * max_distance / r);
        countries[i].y = y + 1.05 * r * std::sin(angle + step_size * max_distance / r);
    }
}
void apply_twist_and_stretch_negative(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours) {
    double x{ 0 };
    double y{ 0 };
    for (int i = 0; i < countries.size(); ++i) {
        x += countries[i].x;
        y += countries[i].y;
    }
    x = x / countries.size();
    y = y / countries.size();
    double max_distance{ std::numeric_limits<double>::min() };
    for (int i = 0; i < countries.size(); ++i) {
        const double dx{ countries[i].x - x };
        const double dy{ countries[i].y - y };
        const double r = std::sqrt(dx * dx + dy * dy);
        max_distance = std::max(max_distance, r);
    }
    const double step_size{ 2 * PI * 0.01 };
    for (int i = 0; i < countries.size(); ++i) {
        const double dx{ countries[i].x - x };
        const double dy{ countries[i].y - y };
        const double r = std::sqrt(dx * dx + dy * dy);
        const double angle{ get_angle(dx,dy) };
        countries[i].x = x + 1.05 * r * std::cos(angle - step_size * max_distance / r);
        countries[i].y = y + 1.05 * r * std::sin(angle - step_size * max_distance / r);
    }
}
void apply_twist_and_compress(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours) {
    double x{ 0 };
    double y{ 0 };
    for (int i = 0; i < countries.size(); ++i) {
        x += countries[i].x;
        y += countries[i].y;
    }
    x = x / countries.size();
    y = y / countries.size();
    double max_distance{ std::numeric_limits<double>::min() };
    for (int i = 0; i < countries.size(); ++i) {
        const double dx{ countries[i].x - x };
        const double dy{ countries[i].y - y };
        const double r = std::sqrt(dx * dx + dy * dy);
        max_distance = std::max(max_distance, r);
    }
    const double step_size{ 2 * PI * 0.01 };
    for (int i = 0; i < countries.size(); ++i) {
        const double dx{ countries[i].x - x };
        const double dy{ countries[i].y - y };
        const double r = std::sqrt(dx * dx + dy * dy);
        const double angle{ get_angle(dx,dy) };
        countries[i].x = x + 0.97 * r * std::cos(angle + step_size * max_distance / r);
        countries[i].y = y + 0.97 * r * std::sin(angle + step_size * max_distance / r);
    }
}
void apply_twist_and_compress_negative(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours) {
    double x{ 0 };
    double y{ 0 };
    for (int i = 0; i < countries.size(); ++i) {
        x += countries[i].x;
        y += countries[i].y;
    }
    x = x / countries.size();
    y = y / countries.size();
    double max_distance{ std::numeric_limits<double>::min() };
    for (int i = 0; i < countries.size(); ++i) {
        const double dx{ countries[i].x - x };
        const double dy{ countries[i].y - y };
        const double r = std::sqrt(dx * dx + dy * dy);
        max_distance = std::max(max_distance, r);
    }
    const double step_size{ 2 * PI * 0.01 };
    for (int i = 0; i < countries.size(); ++i) {
        const double dx{ countries[i].x - x };
        const double dy{ countries[i].y - y };
        const double r = std::sqrt(dx * dx + dy * dy);
        const double angle{ get_angle(dx,dy) };
        countries[i].x = x + 0.97 * r * std::cos(angle - step_size * max_distance / r);
        countries[i].y = y + 0.97 * r * std::sin(angle - step_size * max_distance / r);
    }
}
void apply_zoom_out(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours) {
    for (int i = 0; i < countries.size(); ++i) {
        countries[i].x *= 1.05;
        countries[i].y *= 1.05;
    }
}
void apply_zoom_in(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours) {
    for (int i = 0; i < countries.size(); ++i) {
        countries[i].x *= 0.98;
        countries[i].y *= 0.98;
    }
}
void apply_skew_x(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours) {
    double y{ 0 };
    for (int i = 0; i < countries.size(); ++i) {
        y += countries[i].y;
    }
    y = y / countries.size();
    for (int i = 0; i < countries.size(); ++i) {
        countries[i].x += 0.1 * (countries[i].y - y);
    }
}
void apply_skew_x_negative(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours) {
    double y{ 0 };
    for (int i = 0; i < countries.size(); ++i) {
        y += countries[i].y;
    }
    y = y / countries.size();
    for (int i = 0; i < countries.size(); ++i) {
        countries[i].x -= 0.1 * (countries[i].y - y);
    }
}
void apply_skew_y(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours) {
    double x{ 0 };
    for (int i = 0; i < countries.size(); ++i) {
        x += countries[i].x;
    }
    x = x / countries.size();
    for (int i = 0; i < countries.size(); ++i) {
        countries[i].y += 0.1 * (countries[i].x - x);
    }
}
void apply_skew_y_negative(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours) {
    double x{ 0 };
    for (int i = 0; i < countries.size(); ++i) {
        x += countries[i].x;
    }
    x = x / countries.size();
    for (int i = 0; i < countries.size(); ++i) {
        countries[i].y -= 0.1 * (countries[i].x - x);
    }
}
void apply_decrease_radii(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours) {
    for (int i = 0; i < countries.size(); ++i) {
        countries[i].value *= 0.90;
    }
}
void apply_increase_radii(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours) {
    for (int i = 0; i < countries.size(); ++i) {
        countries[i].value *= 1.05;
    }
}
void apply_nothing(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours) {}
inline double avg_relative_distance(const vector<CountryData>& c, const vector<vector<size_t>>& n, const size_t k) {
    double ret{ 0 };
    for (int i = 0; i < c.size(); ++i) {
        for (int j = 0; j < n[i].size(); ++j) {
            const double dx{ c[n[i][j]].x - c[i].x };
            const double dy{ c[n[i][j]].y - c[i].y };
            const double ideal_distance{ c[n[i][j]].value + c[i].value };
            const double d{ std::sqrt(dx * dx + dy * dy) };
            const double diff{ d - ideal_distance };
            const double rel_dist{ (diff + abs(diff))/ideal_distance };
            ret += rel_dist;
        }
    }
    return ret * 50 / k;
}
inline double avg_relative_angle_delta(const vector<CountryData>& c, const vector<vector<size_t>>& n, const vector<vector<double>>& initialAngles, const size_t k) {
    double ret{ 0 };
    for (int i = 0; i < c.size(); ++i) {
        for (int j = 0; j < n[i].size(); ++j) {
            const double dx{ c[n[i][j]].x - c[i].x };
            const double dy{ c[n[i][j]].y - c[i].y };
            const double angle{ get_angle(dx,dy) };
            const double rel_angle{ abs(abs(angle - initialAngles[i][j]) - PI) };
            ret += rel_angle;
        }
    }
    return 100 - 100 / PI * ret / k;
}
inline double get_score(const vector<CountryData>& c, const vector<vector<size_t>>& n, const vector<vector<double>>& initialAngles, const size_t k) {
    double overlap{ 0 };
    for (int i = 0; i < c.size(); ++i) {
        for (int j = i + 1; j < c.size(); ++j) {
            overlap = std::max(overlap, get_relative_overlap(c[j].x - c[i].x, c[j].y - c[i].y, c[i].value, c[j].value));
        }
    }
    const double distance{ avg_relative_distance(c,n,k) };
    const double angle_delta{ avg_relative_angle_delta(c,n,initialAngles,k) };
    return 1000 * (c.size() + k) / (1 + 0.1 * overlap * overlap + 0.05 * distance * distance + 0.05 * angle_delta * angle_delta);
}
inline double get_score(const vector<CountryData>& c, const vector<vector<size_t>>& n, const vector<vector<double>>& initialAngles, const size_t k, const bool print) {
    double overlap{ 0 };
    for (int i = 0; i < c.size(); ++i) {
        for (int j = i + 1; j < c.size(); ++j) {
            overlap = std::max(overlap, get_relative_overlap(c[j].x - c[i].x, c[j].y - c[i].y, c[i].value, c[j].value));
        }
    }
    const double distance{ avg_relative_distance(c,n,k) };
    const double angle_delta{ avg_relative_angle_delta(c,n,initialAngles,k) };
    if (print) {
        cout << "overlap: " << overlap << "\ndistance: " << distance << "\nangle: " << angle_delta << "\n";
    }
    return 1000 * (c.size() + k) / (1 + 0.1 * overlap * overlap + 0.05 * distance * distance + 0.05 * angle_delta * angle_delta);
}
pair<size_t, size_t> get_overlap_indices(const vector<CountryData>& countries) {
    pair<size_t, size_t> ret{0,0};
    double overlap{ std::numeric_limits<double>::min() };
    for (int i = 0; i < countries.size(); ++i) {
        for (int j = i + 1; j < countries.size(); ++j) {
            const double val{ get_relative_overlap(countries[j].x - countries[i].x, countries[j].y - countries[i].y, countries[i].value, countries[j].value) };
            if (val > overlap) {
                overlap = val;
                ret.first = i;
                ret.second = j;
            }
        }
    }
    return ret;
}
void normalizeRadiiOptimal(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours, const vector<vector<double>>& initialAngles, const size_t k, const vector<string>& names, const string& file_name) {
    vector<CountryData> current_best{ countries };
    double best_score{ get_score(countries,neighbours,initialAngles,k) };
    vector<CountryData> base{ countries };
    size_t improvement_counter{ 0 };
    for (int i = 0; i < 1000; ++i) {
        for (int j = 0; j < countries.size(); ++j) {
            countries[j].value = base[j].value * (0.5 + 0.001 * i);
        }
        const double score{ get_score(countries,neighbours,initialAngles,k) };
        if (score > best_score) {
            current_best = countries;
            best_score = score;
            ++improvement_counter;
        }
    }
    countries = current_best;
}
vector<pair<size_t, vector<size_t>>> get_free_groups(const string& file_name) {
    if (file_name == "Population_Density_Americas.txt" || file_name == "Population_Americas.txt" || file_name == "Area_Americas.txt") {
        return { {12,{9,19}}, {4,{7,11,15,5,12,9,19}}, {13,{7,11,15,5,12,9,19,4}}, {14,{7,11,15,5,12,9,19,4,13}}, {20,{7,11,15,5,12,9,19,4,13,14}}, {2, {21, 22, 1, 16}} };
    }
    else if (file_name == "Area_Europe.txt") {
        return { {8, {12,17}} };
    }
    else if (file_name == "Area_Asia.txt") {
        return { {7, {32, 17}}, {31, {38, 12}}, {6, {31, 38, 12, 3}}, {28, {9, 30, 1}}, {29, {4, 8, 20, 41, 13, 40, 19, 25, 27, 11, 35, 28, 9, 30, 1, 33}} };
    }
    else if (file_name == "Area_Afro_Eurasia.txt") {
        return { {68, {52, 127, 114}}, {11, {96, 41, 34, 122, 90}}, { 96, {34, 122, 90}}, { 34, {122, 90}}, {64, {72, 112, 107, 28, }} };
    }
    else if (file_name == "Population_Afro-Eurasia" || file_name == "Population_Density_Afro-Eurasia") {
        return { {11, {96, 41, 34, 122, 90}}, {96,{34,122,90}}, {34,{122,90}} };
    }
    else if (file_name == "GNI_per_capita_Afro-Eurasia.txt") {
        return { {60, {105, 101, 27, 68}},{60, {105, 101, 27}}, {10, {91, 39, 32, 115, 85}}, {91, {32, 115, 85}}, {32, {115, 85}} };
    }
    else if (file_name == "Instant_Noodle_Consumption_Eurasia.txt") {
        return { {18, {1, 26}}, {18, { 0, 2, 3, 4, 8, 9, 10, 13, 14, 16, 20, 24}}, {7, { 17, 25 }}, {11, {5, 19, 22, 23}}, {6, { 0, 2, 3, 4, 8, 9, 10, 13, 14, 16, 18, 20, 24}} };
    }
    else if (file_name == "CO2_Production_Afro-Eurasia.txt") {
        return { {10, {91, 39, 32, 116, 85}}, {91, {32,116,85}} };
    }
    return {};
}
void post_process(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours, const vector<vector<double>>& initialAngles, const size_t k, const vector<string> names, const string file_name, const bool print=false) {
    vector<CountryData> current_best{ countries };
    double best_score{ get_score(countries,neighbours,initialAngles,k) };
    vector<CountryData> base{ countries };
    size_t improvement_counter{ 0 };
    bool first{ true };
    double score_diff{ 0 };
    vector<pair<size_t, vector<size_t>>> groups{ get_free_groups(file_name) };
    while (score_diff > 0.1 || first) {
        //if (print) cout << file_name << " started new post process iteration\n";
        first = false;
        const double start_score{ best_score };
        bool improved{ true };
        double last_printed_score{ best_score };
        while (improved && improvement_counter < 500) {
            improved = false;
            base = current_best;
            for (int i = 0; i < countries.size(); ++i) {
                for (int j = 0; j < neighbours[i].size(); ++j) {
                    countries = base;
                    const double dx{ base[neighbours[i][j]].x - base[i].x };
                    const double dy{ base[neighbours[i][j]].y - base[i].y };
                    const double r = std::sqrt(dx * dx + dy * dy);
                    const double angle{ get_angle(dx,dy) };
                    for (int l = 1; l < 180; ++l) {
                        countries[neighbours[i][j]].x = base[i].x + r * std::cos(angle + 2 * l * 2*PI/360);
                        countries[neighbours[i][j]].y = base[i].y + r * std::sin(angle + 2 * l * 2*PI/360);
                        const double score{ get_score(countries,neighbours,initialAngles,k) };
                        if (score > best_score) {
                            current_best = countries;
                            best_score = score;
                            improved = true;
                            ++improvement_counter;
                            if (print && improvement_counter % 100 == 0) {
                                cout << "Improvement by " << score - last_printed_score << " " << file_name << " (" << score << " total) after "<<improvement_counter<<" improvements\n";
                                last_printed_score = score;
                            }
                        }
                    }
                }
            }
        }
        countries = current_best;
        for (int i = 0; i < 1000; ++i) {
            for (int j = 0; j < countries.size(); ++j) {
                countries[j].value = base[j].value * (0.5 + 0.001 * i);
            }
            const double score{ get_score(countries,neighbours,initialAngles,k) };
            if (score > best_score) {
                current_best = countries;
                best_score = score;
                ++improvement_counter;
                if (print) {
                    cout << "Improvement by " << score - last_printed_score << " " << file_name << " (" << score << " total) after " << improvement_counter << " radius improvements\n";
                    last_printed_score = score;
                }
            }
        }
        countries = current_best;
        improved = true;
        improvement_counter = 0;
        while (improved && (improvement_counter < 500 || print)) {
            improved = false;
            base = current_best;
            const double start_score{ get_score(base,neighbours,initialAngles,k) };
            for (int i = 0; i < countries.size(); ++i) {
                for (int j = 0; j < neighbours[i].size(); ++j) {
                    countries = base;
                    const double dx{ base[neighbours[i][j]].x - base[i].x };
                    const double dy{ base[neighbours[i][j]].y - base[i].y };
                    const double r = std::sqrt(dx * dx + dy * dy);
                    const double angle{ get_angle(dx,dy) };
                    for (int l = 1; l < 250; ++l) {
                        countries[neighbours[i][j]].x = base[i].x + r * (0.875 + l / 1000.0) * std::cos(angle);
                        countries[neighbours[i][j]].y = base[i].y + r * (0.875 + l / 1000.0) * std::sin(angle);
                        const double score{ get_score(countries,neighbours,initialAngles,k) };
                        if (score > best_score) {
                            current_best = countries;
                            best_score = score;
                            improved = true;
                            ++improvement_counter;
                            if (print && improvement_counter % 100 == 0) {
                                cout << "Improvement by " << score - last_printed_score << " " << file_name << " (" << score << " total) after " << improvement_counter << " distance improvements\n";
                                last_printed_score = score;
                                write_result(current_best, names, file_name);
                            }
                        }
                    }
                }
            }
            if(print) write_result(current_best, names, file_name);
            const double end_score{ get_score(current_best,neighbours,initialAngles,k) };
            if (end_score - start_score < 1) break;
        }
        countries = current_best;
        improved = true;
        improvement_counter = 0;
        while (improved && (improvement_counter < 500 || print)) {
            improved = false;
            base = current_best;
            const double start_score{ get_score(base,neighbours,initialAngles,k) };
            for (const auto& group : groups) {
                countries = base;
                double x{ 0 };
                double y{ 0 };
                for (int i = 0; i < group.second.size(); ++i) {
                    x += countries[group.second[i]].x;
                    y += countries[group.second[i]].y;
                }
                x /= group.second.size();
                y /= group.second.size();
                vector<pair<double, double>> offsets{};
                for (int i = 0; i < group.second.size(); ++i) {
                    offsets.push_back({ countries[group.second[i]].x - x, countries[group.second[i]].y - y });
                }
                const double dx{ x - countries[group.first].x };
                const double dy{ y - countries[group.first].y };
                const double r{ std::sqrt(dx * dx + dy * dy) };
                const double angle{ get_angle(dx,dy) };
                for (int l = 1; l < 250; ++l) {
                    const double offsetX{ r * (l - 125) / 10000.0 * std::cos(angle) };
                    const double offsetY{ r * (l - 125) / 10000.0 * std::sin(angle) };
                    for (int i = 0; i < group.second.size(); ++i) {
                        countries[group.second[i]].x = x + offsetX + offsets[i].first;
                        countries[group.second[i]].y = y + offsetY + offsets[i].second;
                    }
                    const double score{ get_score(countries,neighbours,initialAngles,k) };
                    if (score > best_score) {
                        current_best = countries;
                        best_score = score;
                        improved = true;
                        ++improvement_counter;
                        if (print && improvement_counter % 100 == 0) {
                            cout << "Improvement by " << score - last_printed_score << " " << file_name << " (" << score << " total) after " << improvement_counter << " group distance improvements\n";
                            last_printed_score = score;
                            write_result(current_best, names, file_name);
                        }
                    }
                }
            }
            const double end_score{ get_score(current_best,neighbours,initialAngles,k) };
            if (end_score - start_score < 1) break;
        }
        countries = current_best;
        improved = true;
        improvement_counter = 0;
        while (improved && (improvement_counter < 500 || print)) {
            improved = false;
            base = current_best;
            const double start_score{ get_score(base,neighbours,initialAngles,k) };
            for (const auto& group : groups) {
                countries = base;
                double x{ 0 };
                double y{ 0 };
                for (int i = 0; i < group.second.size(); ++i) {
                    x += countries[group.second[i]].x;
                    y += countries[group.second[i]].y;
                }
                x /= group.second.size();
                y /= group.second.size();
                vector<pair<double, double>> offsets{};
                for (int i = 0; i < group.second.size(); ++i) {
                    offsets.push_back({ countries[group.second[i]].x - x, countries[group.second[i]].y - y });
                }
                const double dx{ x - countries[group.first].x };
                const double dy{ y - countries[group.first].y };
                const double r{ std::sqrt(dx * dx + dy * dy) };
                const double angle{ get_angle(dx,dy) };
                for (int l = 1; l < 250; ++l) {
                    const double offsetAngle{ ((l - 125) / 1000.0) };
                    const double offsetX{ r * std::cos(angle + offsetAngle) };
                    const double offsetY{ r * std::sin(angle + offsetAngle) };
                    for (int i = 0; i < group.second.size(); ++i) {
                        countries[group.second[i]].x = base[group.first].x + offsetX + offsets[i].first;
                        countries[group.second[i]].y = base[group.first].y + offsetY + offsets[i].second;
                    }
                    const double score{ get_score(countries,neighbours,initialAngles,k) };
                    if (score > best_score) {
                        current_best = countries;
                        best_score = score;
                        improved = true;
                        ++improvement_counter;
                        if (print && improvement_counter % 100 == 0) {
                            cout << "Improvement by " << score - last_printed_score << " " << file_name << " (" << score << " total) after " << improvement_counter << " group angle improvements\n";
                            last_printed_score = score;
                            write_result(current_best, names, file_name);
                        }
                    }
                }
            }
            const double end_score{ get_score(current_best,neighbours,initialAngles,k) };
            if (end_score - start_score < 1) break;
        }
        countries = current_best;
        score_diff = get_score(countries, neighbours, initialAngles, k) - start_score;
        //if (print) cout << file_name << " ended iteration with score diff of " << score_diff << endl;
    }
}
void gradient_descent(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours, const vector<vector<double>>& initialAngles, const size_t k, const vector<string>& names, const string& file_name) {
    const size_t max_iterations{ 100 };
    vector<double>base_radii{};
    for (int i = 0; i < countries.size(); ++i) {
        base_radii.push_back(countries[i].value);
    }
    vector<CountryData> currentBest{ countries };
    double current_best_score{ get_score(countries, neighbours, initialAngles, k) };
    double step_size{ 1 };
    size_t iterations{ 0 };
    double epsilon{ 1 };
    double last_score{ current_best_score };
    vector<double> zeroVec{};
    for (int i = 0; i < countries.size(); ++i) {
        zeroVec.push_back(0);
    }
    vector<double> last_gradX{ zeroVec };
    vector<double> last_gradY{ zeroVec };
    double last_gradR{ 0 };
    while (iterations++ < max_iterations) {
        vector<double> gradX{ zeroVec };
        vector<double> gradY{ zeroVec };
        const pair<size_t, size_t> overlap_indices{ get_overlap_indices(countries) };
        CountryData c1{ countries[overlap_indices.first] };
        CountryData c2{ countries[overlap_indices.second] };
        double dx{ c2.x - c1.x };
        double dy{ c2.y - c1.y };
        const double overlap{ get_relative_overlap(dx, dy, c1.value, c2.value) };
        double oGradX;
        double oGradY;
        double gradR;
        double absOfRVector;
        double factor;
        if (overlap <= 0) {
            gradR = 0;
            oGradX = 0;
            oGradY = 0;
        }
        else {
            absOfRVector = std::sqrt(dx * dx + dy * dy);
            factor = -100 * 0.1 * 2 * overlap / ((c1.value + c2.value) * absOfRVector);
            oGradX = dx * factor;
            oGradY = dy * factor;
            if (dx == 0 || dy == 0) {
                oGradX = -2 * overlap * dx / (c1.value + c2.value);
                oGradY = -2 * overlap * dy / (c1.value + c2.value);
            }
            gradR = -100 * 0.1 * 2 * overlap * absOfRVector * signum(epsilon) / ((c1.value + c2.value) * (epsilon * epsilon));
        }
        gradX[overlap_indices.first] += oGradX;
        gradY[overlap_indices.first] += oGradY;
        gradX[overlap_indices.second] -= oGradX;
        gradY[overlap_indices.second] -= oGradY;
        const double avg_dist{ avg_relative_distance(countries, neighbours, k) };
        const double avg_angle{ avg_relative_angle_delta(countries, neighbours, initialAngles, k) };
        double absGrad{ 0 };
        double partialGradR{ 0 };
        for (int i = 0; i < countries.size(); ++i) {
            double x_dist{ 0 };
            double y_dist{ 0 };
            double x_angle{ 0 };
            double y_angle{ 0 };
            double r{ 0 };
            for (int j = 0; j < neighbours[i].size(); ++j) {
                const size_t neighbour{ neighbours[i][j] };
                double dx{ countries[neighbour].x - countries[i].x };
                double dy{ countries[neighbour].y - countries[i].y };
                const double dist{ std::sqrt(dx * dx + dy * dy) };
                const double sum_radii{ countries[i].value + countries[neighbour].value };
                double factor{ 100 / (sum_radii * dist) };
                double dGradX{ dx * factor };
                double dGradY{ dy * factor };
                double dGradR{ dist / sum_radii * (1 + signum(dist / sum_radii - 1)) };
                if (get_relative_distance(dx, dy, countries[i].value, countries[neighbour].value) <= 0) {
                    dGradX = 0;
                    dGradY = 0;
                }
                else if (dx == 0 && dy == 0) {
                    dGradX = 100 / sum_radii; 
                    dGradY = 100 / sum_radii;
                }
                x_dist += dGradX;
                y_dist += dGradY;
                r += dGradR;
                double d_angle{ angle_delta(get_angle(dx,dy), initialAngles[i][j]) };
                factor = -10 / PI * signum(abs(d_angle) - PI) * signum(d_angle) / (1 + dy * dy / (dx * dx));
                double aGradX{ -dy * factor / (dx * dx) };
                double aGradY{ factor / dx };
                if (dx == 0) {
                    aGradX = -dy * 10 / PI * signum(abs(d_angle) - PI) * signum(d_angle);
                    aGradY = 0;
                }
                x_angle += aGradX;
                y_angle += aGradY;
            }
            const double x{ 0.1 / k * (avg_dist * x_dist + avg_angle * x_angle) };
            const double y{ 0.1 / k * (avg_dist * y_dist + avg_angle * y_angle) };
            gradX[i] += x;
            gradY[i] += y;
            partialGradR += r;
            absGrad += x * x + y * y;
        }
        gradR += -50 * partialGradR / (k * epsilon);
        absGrad += gradR * gradR;
        absGrad = std::sqrt(absGrad);
        const double base_r{ epsilon };
        for (int i = 0; i < countries.size(); ++i) {
            gradX[i] = gradX[i] / absGrad;
            gradY[i] = gradY[i] / absGrad;
        }
        gradR = gradR / absGrad;
        const vector<CountryData>base{ countries }; 
        double a{ 0 };
        double b{ 0.02 };
        double c;
        double score_of_lower_bound{ get_score(countries,neighbours,initialAngles,k) };
        while (b - a > 0.000000000001) {
            c = step_size * (b + a) * 0.5;
            countries = base;
            epsilon = base_r;
            for (int i = 0; i < countries.size(); ++i) {
                countries[i].x += c * gradX[i];
                countries[i].y += c * gradY[i];
            }
            epsilon += c * gradR;
            for (int i = 0; i < countries.size(); ++i) {
                countries[i].value = abs(epsilon) * base_radii[i];
            }
            const double score_after{ get_score(countries, neighbours, initialAngles, k) };
            if (score_after > score_of_lower_bound) {
                a = c;
                score_of_lower_bound = score_after;
            }
            else if (score_after < score_of_lower_bound) {
                b = c;
            }
            if (score_after == score_of_lower_bound) break;
        }
        const double score{ get_score(countries,neighbours,initialAngles, k) };
        if (score > current_best_score) {
            current_best_score = score;
            currentBest = countries;
        }
        if (score > last_score) {
            --iterations;
        }
        else step_size *= 0.95;
        last_score = score;
    }
    countries = currentBest;
}
void gradient_heavy_ball(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours, const vector<vector<double>>& initialAngles, const size_t k, const vector<string>& names, const string& file_name) {
    const size_t max_iterations{ 100 };
    vector<double>base_radii{};
    for (int i = 0; i < countries.size(); ++i) {
        base_radii.push_back(countries[i].value);
    }
    vector<CountryData> currentBest{ countries };
    double current_best_score{ get_score(countries, neighbours, initialAngles, k) };
    double step_size{ 1 };
    size_t iterations{ 0 };
    double epsilon{ 1 };
    double last_score{ current_best_score };
    vector<double> zeroVec{};
    for (int i = 0; i < countries.size(); ++i) {
        zeroVec.push_back(0);
    }
    vector<double> momentumX{ zeroVec };
    vector<double> momentumY{ zeroVec };
    double momentumR{ 0 };
    const double a{ 0.5 };
    const double b{ 0.85 * a };
    while (iterations++ < max_iterations) {
        vector<double> gradX{ zeroVec };
        vector<double> gradY{ zeroVec };
        const pair<size_t, size_t> overlap_indices{ get_overlap_indices(countries) };
        CountryData c1{ countries[overlap_indices.first] };
        CountryData c2{ countries[overlap_indices.second] };
        double dx{ c2.x - c1.x };
        double dy{ c2.y - c1.y };
        const double overlap{ get_relative_overlap(dx, dy, c1.value, c2.value) };
        double oGradX;
        double oGradY;
        double gradR;
        double absOfRVector;
        double factor;
        if (overlap <= 0) {
            gradR = 0;
            oGradX = 0;
            oGradY = 0;
        }
        else {
            absOfRVector = std::sqrt(dx * dx + dy * dy);
            factor = -100 * 0.1 * 2 * overlap / ((c1.value + c2.value) * absOfRVector);
            oGradX = dx * factor;
            oGradY = dy * factor;
            if (dx == 0 || dy == 0) {
                oGradX = -2 * overlap * dx / (c1.value + c2.value);
                oGradY = -2 * overlap * dy / (c1.value + c2.value);
            }
            gradR = -100 * 0.1 * 2 * overlap * absOfRVector * signum(epsilon) / ((c1.value + c2.value) * (epsilon * epsilon));
        }
        gradX[overlap_indices.first] += oGradX;
        gradY[overlap_indices.first] += oGradY;
        gradX[overlap_indices.second] -= oGradX;
        gradY[overlap_indices.second] -= oGradY;
        const double avg_dist{ avg_relative_distance(countries, neighbours, k) };
        const double avg_angle{ avg_relative_angle_delta(countries, neighbours, initialAngles, k) };
        double absGrad{ 0 };
        double partialGradR{ 0 };
        for (int i = 0; i < countries.size(); ++i) {
            double x_dist{ 0 };
            double y_dist{ 0 };
            double x_angle{ 0 };
            double y_angle{ 0 };
            double r{ 0 };
            for (int j = 0; j < neighbours[i].size(); ++j) {
                const size_t neighbour{ neighbours[i][j] };
                double dx{ countries[neighbour].x - countries[i].x };
                double dy{ countries[neighbour].y - countries[i].y };
                const double dist{ std::sqrt(dx * dx + dy * dy) };
                const double sum_radii{ countries[i].value + countries[neighbour].value };
                double factor{ 100 / (sum_radii * dist) };
                double dGradX{ dx * factor };
                double dGradY{ dy * factor };
                double dGradR{ dist / sum_radii * (1 + signum(dist / sum_radii - 1)) };
                if (get_relative_distance(dx, dy, countries[i].value, countries[neighbour].value) <= 0) {
                    dGradX = 0;
                    dGradY = 0;
                }
                else if (dx == 0 && dy == 0) {
                    dGradX = 100 / sum_radii;
                    dGradY = 100 / sum_radii;
                }
                x_dist += dGradX;
                y_dist += dGradY;
                r += dGradR;
                double d_angle{ angle_delta(get_angle(dx,dy), initialAngles[i][j]) };
                factor = -10 / PI * signum(abs(d_angle) - PI) * signum(d_angle) / (1 + dy * dy / (dx * dx));
                double aGradX{ -dy * factor / (dx * dx) };
                double aGradY{ factor / dx };
                if (dx == 0) {
                    aGradX = -dy * 10 / PI * signum(abs(d_angle) - PI) * signum(d_angle);
                    aGradY = 0;
                }
                x_angle += aGradX;
                y_angle += aGradY;
            }
            const double x{ 0.1 / k * (avg_dist * x_dist + avg_angle * x_angle) };
            const double y{ 0.1 / k * (avg_dist * y_dist + avg_angle * y_angle) };
            gradX[i] += x;
            gradY[i] += y;
            partialGradR += r;
            absGrad += x * x + y * y;
        }
        gradR *= -50 * partialGradR / (k * epsilon);
        absGrad += gradR * gradR;
        absGrad = std::sqrt(absGrad);
        const double base_r{ epsilon };
        gradR = gradR / absGrad;
        for (int i = 0; i < countries.size(); ++i) {
            const double x{ a * gradX[i] / absGrad + b * momentumX[i] };
            const double y{ a * gradY[i] / absGrad + b * momentumY[i] };
            countries[i].x += x;
            countries[i].y += y;
            momentumX[i] = x;
            momentumY[i] = y;
        }
        const double r{ a * gradR + b * momentumR };
        epsilon += r;
        momentumR = r;
        for (int i = 0; i < countries.size(); ++i) {
            countries[i].value = abs(epsilon) * base_radii[i];
        }
        const double score{ get_score(countries,neighbours,initialAngles, k) };
        if (score > current_best_score) {
            current_best_score = score;
            currentBest = countries;
        }
        if (score > last_score) {
            --iterations;
        }
        else step_size *= 0.95;
        last_score = score;
    }
    countries = currentBest;
}
void gradient_descent_old(vector<CountryData>& countries, const vector<vector<size_t>>& neighbours, const vector<vector<double>>& initialAngles, const size_t k, const vector<string>& names, const string& file_name) {
    const size_t thread_id{ (size_t)omp_get_thread_num() };
    const size_t max_iterations{ 10000 };
    vector<double>base_radii{};
    for (int i = 0; i < countries.size(); ++i) {
        base_radii.push_back(countries[i].value);
    }
    vector<CountryData> currentBest{ countries };
    double current_best_score{ get_score(countries, neighbours, initialAngles, k) };
    if (true) {
        double base_step_size{ 0.00003125 };
        size_t max_index{ 81 };
        const vector<CountryData> base{ countries };
        vector<double> zeroVec{};
        for (int i = 0; i < countries.size(); ++i) {
            zeroVec.push_back(0);
        }
        for (int i = 1; i < max_index; ++i) {
            double step_size{ i * base_step_size };
            size_t iterations{ 0 };
            size_t score_increase_counter{ 0 };
            double epsilon{ 1 };
            while (iterations++ < max_iterations) {
                vector<double> gradX{ zeroVec };
                vector<double> gradY{ zeroVec };
                const pair<size_t, size_t> overlap_indices{ get_overlap_indices(countries) };
                CountryData c1{ countries[overlap_indices.first] };
                CountryData c2{ countries[overlap_indices.second] };
                double dx{ c2.x - c1.x };
                double dy{ c2.y - c1.y };
                const double overlap{ get_relative_overlap(dx, dy, c1.value, c2.value) };
                double oGradX;
                double oGradY;
                double gradR;
                double absOfRVector;
                double factor;
                if (overlap <= 0) {
                    gradR = 0;
                    oGradX = 0;
                    oGradY = 0;
                }
                else {
                    absOfRVector = std::sqrt(dx * dx + dy * dy);
                    factor = -100 * 0.1 * 2 * overlap / ((c1.value + c2.value) * absOfRVector);
                    oGradX = dx * factor;
                    oGradY = dy * factor;
                    if (dx == 0 || dy == 0) {
                        oGradX = -2 * overlap * dx / (c1.value + c2.value);
                        oGradY = -2 * overlap * dy / (c1.value + c2.value);
                    }
                    gradR = -100 * 0.1 * 2 * overlap * absOfRVector * signum(epsilon) / ((c1.value + c2.value) * (epsilon * epsilon));
                }
                gradX[overlap_indices.first] += oGradX;
                gradY[overlap_indices.first] += oGradY;
                gradX[overlap_indices.second] -= oGradX;
                gradY[overlap_indices.second] -= oGradY;
                const double avg_dist{ avg_relative_distance(countries, neighbours, k) };
                const double avg_angle{ avg_relative_angle_delta(countries, neighbours, initialAngles, k) };
                double absGrad{ 0 };
                double partialGradR{ 0 };
                for (int i = 0; i < countries.size(); ++i) {
                    double x_dist{ 0 };
                    double y_dist{ 0 };
                    double x_angle{ 0 };
                    double y_angle{ 0 };
                    double r{ 0 };
                    for (int j = 0; j < neighbours[i].size(); ++j) {
                        const size_t neighbour{ neighbours[i][j] };
                        double dx{ countries[neighbour].x - countries[i].x };
                        double dy{ countries[neighbour].y - countries[i].y };
                        const double dist{ std::sqrt(dx * dx + dy * dy) };
                        const double sum_radii{ countries[i].value + countries[neighbour].value };
                        double factor{ 100 / (sum_radii * dist) };
                        double dGradX{ dx * factor };
                        double dGradY{ dy * factor };
                        double dGradR{ dist / sum_radii * (1+signum(dist/sum_radii - 1))};
                        if (get_relative_distance(dx, dy, countries[i].value, countries[neighbour].value) <= 0) {
                            dGradX = 0;
                            dGradY = 0;
                        }
                        else if (dx == 0 && dy == 0) {
                            dGradX = 100 / sum_radii;
                            dGradY = 100 / sum_radii;
                        }
                        x_dist += dGradX;
                        y_dist += dGradY;
                        r += dGradR;
                        double d_angle{ angle_delta(get_angle(dx,dy), initialAngles[i][j]) };
                        factor = -10 / PI * signum(abs(d_angle) - PI) * signum(d_angle) / (1 + dy * dy / (dx * dx));
                        double aGradX{ -dy * factor / (dx * dx) };
                        double aGradY{ factor / dx };
                        if (dx == 0) {
                            aGradX = -dy * 10 / PI * signum(abs(d_angle) - PI) * signum(d_angle);
                            aGradY = 0;
                        }
                        x_angle += aGradX;
                        y_angle += aGradY;
                    }
                    const double x{ 0.1 / k * (avg_dist * x_dist + avg_angle * x_angle) };
                    const double y{ 0.1 / k * (avg_dist * y_dist + avg_angle * y_angle) };
                    gradX[i] += x;
                    gradY[i] += y;
                    partialGradR += r;
                    absGrad += x * x + y * y;
                }
                gradR *= -50 * partialGradR / (k * epsilon);
                absGrad += gradR * gradR;
                absGrad = std::sqrt(absGrad);
                const double base_r{ epsilon };
                for (int i = 0; i < countries.size(); ++i) {
                    gradX[i] = gradX[i] / absGrad;
                    gradY[i] = gradY[i] / absGrad;
                }
                gradR = gradR / absGrad;
                for (int i = 0; i < countries.size(); ++i) {
                    countries[i].x += step_size * gradX[i];
                    countries[i].y += step_size * gradY[i];
                }
                epsilon += step_size * gradR;
                for (int i = 0; i < countries.size(); ++i) {
                    countries[i].value = abs(epsilon) * base_radii[i];
                }
                const double score{ get_score(countries,neighbours,initialAngles, k) };
                //cout << score << "\n";
                if (score > current_best_score) {
                    --iterations;
                    current_best_score = score;
                    currentBest = countries;
                }
                else step_size *= 0.999999;
            }
        }
        for (int i = 1; i < max_index; ++i) {
            double step_size{ i * base_step_size };
            size_t iterations{ 0 };
            size_t score_increase_counter{ 0 };
            double epsilon{ 1 };
            countries = base;
            while (iterations++ < max_iterations) {
                vector<double> gradX{ zeroVec };
                vector<double> gradY{ zeroVec };
                const pair<size_t, size_t> overlap_indices{ get_overlap_indices(countries) };
                CountryData c1{ countries[overlap_indices.first] };
                CountryData c2{ countries[overlap_indices.second] };
                double dx{ c2.x - c1.x };
                double dy{ c2.y - c1.y };
                const double overlap{ get_relative_overlap(dx, dy, c1.value, c2.value) };
                double oGradX;
                double oGradY;
                double gradR;
                double absOfRVector;
                double factor;
                if (overlap <= 0) {
                    gradR = 0;
                    oGradX = 0;
                    oGradY = 0;
                }
                else {
                    absOfRVector = std::sqrt(dx * dx + dy * dy);
                    factor = -100 * 0.1 * 2 * overlap / ((c1.value + c2.value) * absOfRVector);
                    oGradX = dx * factor;
                    oGradY = dy * factor;
                    if (dx == 0 || dy == 0) {
                        oGradX = -2 * overlap * dx / (c1.value + c2.value);
                        oGradY = -2 * overlap * dy / (c1.value + c2.value);
                    }
                    gradR = -100 * 0.1 * 2 * overlap * absOfRVector * signum(epsilon) / ((c1.value + c2.value) * (epsilon * epsilon));
                }
                gradX[overlap_indices.first] += oGradX;
                gradY[overlap_indices.first] += oGradY;
                gradX[overlap_indices.second] -= oGradX;
                gradY[overlap_indices.second] -= oGradY;
                const double avg_dist{ avg_relative_distance(countries, neighbours, k) };
                const double avg_angle{ avg_relative_angle_delta(countries, neighbours, initialAngles, k) };
                double absGrad{ 0 };
                double partialGradR{ 0 };
                for (int i = 0; i < countries.size(); ++i) {
                    double x_dist{ 0 };
                    double y_dist{ 0 };
                    double x_angle{ 0 };
                    double y_angle{ 0 };
                    double r{ 0 };
                    for (int j = 0; j < neighbours[i].size(); ++j) {
                        const size_t neighbour{ neighbours[i][j] };
                        double dx{ countries[neighbour].x - countries[i].x };
                        double dy{ countries[neighbour].y - countries[i].y };
                        const double dist{ std::sqrt(dx * dx + dy * dy) };
                        const double sum_radii{ countries[i].value + countries[neighbour].value };
                        double factor{ 100 / (sum_radii * dist) };
                        double dGradX{ dx * factor };
                        double dGradY{ dy * factor };
                        double dGradR{ -50 * dist * signum(epsilon) / (sum_radii * (epsilon * epsilon)) };
                        if (get_relative_distance(dx, dy, countries[i].value, countries[neighbour].value) <= 0) {
                            dGradX = 0;
                            dGradY = 0;
                        }
                        else if (dx == 0 && dy == 0) {
                            dGradX = 100 / sum_radii;
                            dGradY = 100 / sum_radii;
                        }
                        x_dist += dGradX;
                        y_dist += dGradY;
                        r += dGradR;
                        double d_angle{ angle_delta(get_angle(dx,dy), initialAngles[i][j]) };
                        factor = -10 / PI * signum(abs(d_angle) - PI) * signum(d_angle) / (1 + dy * dy / (dx * dx));
                        double aGradX{ -dy * factor / (dx * dx) };
                        double aGradY{ factor / dx };
                        if (dx == 0) {
                            aGradX = -dy * 10 / PI * signum(abs(d_angle) - PI) * signum(d_angle);
                            aGradY = 0;
                        }
                        x_angle += aGradX;
                        y_angle += aGradY;
                    }
                    const double x{ 0.1 / k * (avg_dist * x_dist + avg_angle * x_angle) };
                    const double y{ 0.1 / k * (avg_dist * y_dist + avg_angle * y_angle) };
                    gradX[i] += x;
                    gradY[i] += y;
                    partialGradR += r;
                    absGrad += x * x + y * y;
                }
                gradR *= -50 * partialGradR / (k * epsilon);
                absGrad += gradR * gradR;
                absGrad = std::sqrt(absGrad);
                const double base_r{ epsilon };
                for (int i = 0; i < countries.size(); ++i) {
                    gradX[i] = gradX[i] / absGrad;
                    gradY[i] = gradY[i] / absGrad;
                }
                gradR = gradR / absGrad;
                for (int i = 0; i < countries.size(); ++i) {
                    countries[i].x += step_size * gradX[i];
                    countries[i].y += step_size * gradY[i];
                }
                epsilon += step_size * gradR;
                for (int i = 0; i < countries.size(); ++i) {
                    countries[i].value = abs(epsilon) * base_radii[i];
                }
                const double score{ get_score(countries,neighbours,initialAngles, k) };
                //cout << score << "\n";
                if (score > current_best_score) {
                    --iterations;
                    current_best_score = score;
                    currentBest = countries;
                }
                else step_size *= 0.999999;
            }
        }
    }
    
    const double s{ current_best_score };
    post_process(currentBest, neighbours, initialAngles, k, names, file_name);
    countries = currentBest;
    vector<CountryData> tmp{ countries };
    gradient_descent(tmp, neighbours, initialAngles, k, names, file_name);
    post_process(tmp, neighbours, initialAngles, k, names, file_name);
    double fine_score{ get_score(tmp, neighbours, initialAngles, k) };
    if (fine_score > s) {
        cout << thread_id << ": " << s << " --> " << fine_score << "\n";
        currentBest = tmp;
    }
    tmp = currentBest;
    gradient_heavy_ball(tmp, neighbours, initialAngles, k, names, file_name);
    post_process(tmp, neighbours, initialAngles, k, names, file_name);
    fine_score = get_score(tmp, neighbours, initialAngles, k);
    if (fine_score > s) {
        cout << "HEAVY BALL " << thread_id << ": " << s << " --> " << fine_score << "\n";
        currentBest = tmp;
    }
    countries = currentBest;

}
double simulate(vector<CountryData> countries, vector<vector<size_t>>& neighbours, vector<string>& names, const string& file_name, const bool start_from_old_results) {
    const size_t thread_id{ (size_t)omp_get_thread_num() };
    vector<CountryData> start{ countries };
    vector<vector<double>> initialAngles{ calculateInitialAngles(countries,neighbours) };
    size_t k{ 0 };
    for (int i = 0; i < countries.size(); ++i) {
        k += neighbours[i].size();
    }
    if (start_from_old_results) {
        fstream file{ "../../../../result_files/" + file_name + ".out", std::ios::in};
        if (file.is_open()) {
            string dump{};
            for (int i = 0; i < countries.size(); ++i) {
                file >> countries[i].x >> countries[i].y >> countries[i].value >> dump >> dump;
            }
        }
        file.close();
    }
    const double max_possible_score{ 1000.0 * (countries.size() + k)};
    vector<CountryData> current_best{ countries };
    double current_best_score{ get_score(current_best,neighbours,initialAngles, k) };
    double score{ current_best_score };
    Heuristic<jiggle_func> h{ 
        apply_nothing, apply_decrease_radii, apply_increase_radii, apply_skew_x, apply_skew_x_negative, apply_skew_y, apply_skew_y_negative,
        /*apply_forces, apply_forces_negative, apply_forces_old, apply_forces_old_negative, */ apply_twist, apply_twist_negative,
        apply_twist_and_stretch, apply_twist_and_stretch_negative, apply_twist_and_compress, apply_twist_and_compress_negative,
        apply_zoom_out, apply_zoom_in,
    };
    Heuristic<radius_func> rh{ normalizeRadiiNone, normalizeRadiiOptimal, /*normalizeRadii*/ };
    bool first{ true };
    while (true) {
        const double start_score{ get_score(current_best, neighbours, initialAngles, k) };
        vector<CountryData> base{ countries };
        if (first) {
            first = false;
            post_process(countries, neighbours, initialAngles, k, names, file_name,true);
            const double s{ get_score(countries, neighbours, initialAngles, k) };
            if (s > current_best_score) {
                cout << "IMPROVEMENT " << "(by " << s-current_best_score << " points)\t" << file_name << " (" << score << " total)\n";
                current_best = countries;
                current_best_score = s;
                base = countries;
                write_result(current_best, names, file_name);
                cout << "WROTE " << file_name << " to file\n";
            }
            cout << file_name << " exited preprocessing" << endl;
        }
        bool first{ true };
        bool found_new_best{ false };
        vector<vector<CountryData>> bases{};
        for (int i = 0; i < h.size(); ++i) {
            bases.push_back(base);
        }
        for (int i = 0; i < 2; ++i) {
            h.next_iteration();
            for (int j = 0; j < h.size(); ++j) {
                rh.next_iteration();
                jiggle_func func{ h.next()};
                const size_t index{ h.last_served };
                func(bases[index], neighbours);
                const vector<CountryData>base{ bases[index] };
                bool break_after{ false };
                for (int l = 0; l < rh.size(); ++l) {
                    radius_func rf{ rh.next() };
                    const size_t r_index{ rh.last_served };
                    countries = base;
                    rf(countries, neighbours, initialAngles, k, names, file_name);
                    gradient_descent_old(countries, neighbours, initialAngles, k, names, file_name);
                    score = get_score(countries, neighbours, initialAngles, k);
                    if (score > current_best_score) {
                        h.mark_found();
                        rh.mark_found();
                        const double diff{ score - current_best_score };
                        cout << "IMPROVEMENT " << "(by " << diff << " points)\t" << file_name << " (" << score << " total) jiggle func " << index << " radius func " << r_index <<  "\n";
                        current_best_score = score;
                        current_best = countries;
                        found_new_best = found_new_best ||  diff > 1;
                        write_result(current_best, names, file_name);
                        if (diff < 0.1) continue;
                        bool improved{ true };
                        int iterations{ 0 };
                        const double start_score{ score };
                        while (improved) {
                            func(bases[index], neighbours);
                            countries = bases[index];
                            rf(countries,neighbours,initialAngles,k,names,file_name);
                            gradient_descent_old(countries, neighbours, initialAngles, k, names, file_name);
                            score = get_score(countries, neighbours, initialAngles, k);
                            improved = score > current_best_score;
                            if (score > current_best_score) {
                                const double diff{ score - current_best_score };
                                cout << "IMPROVEMENT " << "(by " << diff << " points)\t" << file_name << " (" << score << " total) jiggle func " << index << " radius func " << r_index << " continued force " << iterations++ << "\n";
                                current_best_score = score;
                                current_best = countries;
                                if (diff < 1) break;
                            }
                            write_result(current_best, names, file_name);
                        }
                        //if (file_name == "Area_Afro-Eurasia.txt") {
                        //    found_new_best = true;
                        //    break_after = true;
                        //}
                        if (score - start_score > 100) {
                            cout << "break after" << endl;
                            break_after = true;
                        }
                        break_after = true;
                        //found_new_best = true;
                    }
                }
                cout << "[CURRENT BEST " << thread_id << " " << file_name << "] iteration " << i << " jiggle func " << index << "\n" << get_score(current_best, neighbours, initialAngles, k, true) << " / " << max_possible_score << " (" << 100 * get_score(current_best, neighbours, initialAngles, k) / max_possible_score << "%)" << endl;
                if (break_after) break;
            }
            if (found_new_best) break;
        }
        countries = current_best;
        cout << "[BEST " << thread_id << " " << file_name << "]\n" << get_score(countries, neighbours, initialAngles, k, true) << endl;
        const double end_score{ get_score(current_best, neighbours, initialAngles, k) };
        if (end_score - start_score < 1) {
            break;
        }
        if (!found_new_best) {
            break;
        }
    }
    return current_best_score;
}
int main()
{
    const size_t max_iterations{ 1000 };
    auto fileData{ readInputFiles() };
    const bool obscureResults{ false };
    bool first = true;
    double totalScore{ 0 };
    const bool start_from_old_results{ true };
    //const vector<size_t> runFiles{ 0,0,0,0,0,0,0,0,0,0,0,0 };
    const vector<size_t> runFiles{ 1,1,1,0,1,0,1,0,1,0,1,1 };
//    const vector<size_t> runFiles{ 0,0,0,0,0,1,0,0,0,0,0,0 };
    double start_score{ 0 };
    for (int i = 0; i < fileData.size(); ++i) {
        vector<CountryData> countries{ std::get<0>(fileData[i]) };
        vector<string>& names{ std::get<1>(fileData[i]) };
        vector<vector<size_t>>& neighbours{ std::get<2>(fileData[i]) };
        const string& file_name{ std::get<3>(fileData[i]) };
        vector<vector<double>> initialAngles{ calculateInitialAngles(countries,neighbours) };
        size_t k{ 0 };
        for (int i = 0; i < countries.size(); ++i) {
            k += neighbours[i].size();
        }
        if (start_from_old_results) {
            fstream file{ "../../../../result_files/" + file_name + ".out", std::ios::in };
            if (file.is_open()) {
                string dump{};
                for (int i = 0; i < countries.size(); ++i) {
                    file >> countries[i].x >> countries[i].y >> countries[i].value >> dump >> dump;
                }
            }
            file.close();
        }
        const double max_possible_score{ 1000.0 * (countries.size() + k) };
        const double score{ get_score(countries, neighbours, initialAngles, k, true) };
        start_score += score;
        cout << "filename: " << file_name << "\nscore " << score << " / " << max_possible_score << " (" << 100 * get_score(countries, neighbours, initialAngles, k) / max_possible_score << "%)" << endl;
    }
    cout << "TOTAL START SCORE " << start_score << endl;
#pragma omp parallel for num_threads(7) schedule(dynamic, 1)
    for (int i = 0; i < fileData.size(); ++i) {
        if (!runFiles[i]) continue;
        if (obscureResults) {
            if (!first) break;
            first = false;
        }
        vector<CountryData>& countries{ std::get<0>(fileData[i])};
        vector<string>& names{ std::get<1>(fileData[i]) };
        vector<vector<size_t>>& neighbours{ std::get<2>(fileData[i]) };
        const string& file_name{ std::get<3>(fileData[i]) };
        #pragma omp critical
        {
        cout << "[" << omp_get_thread_num() << " WORKS ON] " << file_name << endl;
        }
        totalScore+=simulate(countries, neighbours, names, file_name, start_from_old_results);
        cout << "[TOTAL CURRENT SCORE] " << totalScore << endl;
    }
    cout << "expected total score: " << totalScore << endl;
}