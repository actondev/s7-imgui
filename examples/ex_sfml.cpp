#include <SFML/Window.hpp>
// I have disabled graphics module from sfml
// I don't really need it for imgui etc
// #include <SFML/Graphics.hpp>


int main()
{
    sf::Window window(sf::VideoMode(200, 200), "SFML works!");
//     sf::CircleShape shape(100.f);
//     shape.setFillColor(sf::Color::Green);

    while (window.isOpen())
    {
        sf::Event event;
        while (window.pollEvent(event))
        {
            if (event.type == sf::Event::Closed)
                window.close();
        }

//         window.clear();
//         window.draw(shape);
        window.display();
    }

    return 0;
}

