
abstract class Shape extends Object
{
    public abstract double getArea();
}

class Rectangle extends Shape
{
    private double _width;
    private double _height;

    public Rectangle(double w, double h)
    {
        _width = w;
        _height = h;
    }

    public double getArea() { return _width * _height; }
}

class Square extends Rectangle
{
    public Square(double side)
    {
        super(side, side);
    }
}

class Circle
{
    private double _radius;

    public Circle(double radius)
    {
        _radius = radius;
    }

    public double getArea() { return _radius * _radius * java.lang.Math.PI; }
}


public class Shapes {
    public static void main(String[] args) {
        System.out.println("Testing");

        Rectangle r = new Rectangle(3, 4);
        Square s = new Square(5);
        Circle c = new Circle(10);

        System.out.println("R: " + r.getArea() + ", S: " + s.getArea() + ", C: " + c.getArea());
    }
}


