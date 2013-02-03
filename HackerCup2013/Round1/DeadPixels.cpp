#include <fstream>
#include <iostream>
#include <cstdlib>
#include <cctype>
#include <cstring>
#include <array>
#include <algorithm>
#include <string>
#include <sstream>
#include <queue>
#include <cassert>
#include <utility>
#include <memory>

using namespace std;

struct Point
{
	long x;
	long y;
	Point() {}
	Point(long x, long y) : x(x), y(y) {}
};

struct Rectangle
{
	Point topLeft;
	Point bottomRight;
	Rectangle(Point tl, Point br) : topLeft(tl), bottomRight(br) {}
	Point size()
	{
		return Point(bottomRight.x - topLeft.x, bottomRight.y - topLeft.y);
	}
	bool intersect(Point p)
	{
		return (p.x >= topLeft.x &&
			p.x <= bottomRight.x &&
			p.y >= topLeft.y &&
			p.y <= bottomRight.y);
	}
};

typedef pair<vector<Point>, vector<Point>> TwoVectors;


TwoVectors divideAtMedian(vector<Point> v, size_t length, size_t median)
{
	TwoVectors ret;
	for (size_t i=0; i<median; i++)
	{
		ret.first.push_back(v[i]);
	}
	for (size_t i=median; i<length; i++)
	{
		ret.second.push_back(v[i]);
	}
	return ret;
}

class KDTreeNode
{
public:
	unique_ptr<KDTreeNode> left;
	unique_ptr<KDTreeNode> right;
	Point data;
	KDTreeNode(int depth, vector<Point> points)
	{
		size_t pointsLength = points.size();
		//assert(pointsLength > 0);

		if (pointsLength == 1)
		{
			data = points[0];
		}
		else
		{
			if (depth % 2 == 0)
			{
				sort(points.begin(), points.end(), [](Point p1, Point p2) {
					return p1.x < p2.x;
				});
			}
			else
			{
				sort(points.begin(), points.end(), [](Point p1, Point p2) {
					return p1.y < p2.y;
				});
			}
			size_t median = pointsLength / 2;
			auto vectors = divideAtMedian(points, pointsLength, median);
			if (depth % 2 == 0)
			{
				data = Point(vectors.second[0].x, -1); //reuse data to store the median X value in the X
			}
			else
			{
				data = Point(vectors.second[0].y, -1); //reuse data to store the median Y value in the X
			}
			left = unique_ptr<KDTreeNode>(new KDTreeNode(depth+1, vectors.first));
			right = unique_ptr<KDTreeNode>(new KDTreeNode(depth+1, vectors.second));
		}
	}

	Point nearest_neighbor(int depth, Point p)
	{
		if (data.y != -1)
		{
			return data;
		}
		else
		{
			if (depth % 2 == 0)
			{
				if (p.x < data.x)
				{
					return left->nearest_neighbor(depth+1, p);
				}
				else
				{
					return right->nearest_neighbor(depth+1, p);
				}
			}
			else
			{
				if (p.y < data.x)
				{
					return left->nearest_neighbor(depth+1, p);
				}
				else
				{
					return right->nearest_neighbor(depth+1, p);
				}
			}
		}
	}
};

int main(int argc, char **argv)
{
	ifstream input("DeadPixels_input.txt", ifstream::in);
	
	string line;

	// get input line count
	getline(input, line);
	istringstream iss(line);
	
	int T;
	iss >> T;

	for (int i=0; i<T; i++)
	{
		getline(input, line);
	 	istringstream iss(line);

		long W, H, P, Q, N, X, Y, a, b, c, d;

		iss >> W; // width of monitor
		iss >> H; // height of monitor
		iss >> P; // width of image
		iss >> Q; // height of image
		iss >> N; // number of dead pixels
		iss >> X; // first dead pixel X
		iss >> Y; // first dead pixel Y
		iss >> a; // a,b,c,d are used in formula for dead pixel location
		iss >> b;
		iss >> c;
		iss >> d;

		//printf("DEBUG: W H P Q N X Y a b c d are: %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld\n", W, H, P, Q, N, X, Y, a, b, c, d);

		vector<Point> deadPixels = vector<Point>(N);
		deadPixels[0].x = X;
		deadPixels[0].y = Y;
		for (long j=1; j<N; j++)
		{
			deadPixels[j].x = (deadPixels[j-1].x * a + deadPixels[j-1].y * b + 1) % W;
			deadPixels[j].y = (deadPixels[j-1].x * c + deadPixels[j-1].y * d + 1) % H;
		}

		// put the dead pixels into a KD-Tree
		//assert(deadPixels.size() == N);
		KDTreeNode root(0, deadPixels);

		long count = 0;

		for (long j=0; j<=W-P; j++)
		{
			for (long k=0; k<=H-Q; k++)
			{
				Point center(j+(P/2), k+(Q/2));
				Point topLeft(j, k);
				Point bottomRight(j+(P-1), k+(Q-1));
				Rectangle img(topLeft, bottomRight);
				Point nearestCenter = root.nearest_neighbor(0, center);
				Point nearestTopLeft = root.nearest_neighbor(0, topLeft);
				Point nearestBottomRight = root.nearest_neighbor(0, bottomRight);
				Point nearestTopRight = root.nearest_neighbor(0, Point(bottomRight.x, topLeft.y));
				Point nearestBottomLeft = root.nearest_neighbor(0, Point(topLeft.x, bottomRight.y));
				if (!img.intersect(nearestCenter) &&
					!img.intersect(nearestTopLeft) &&
					!img.intersect(nearestBottomRight) &&
					!img.intersect(nearestTopRight) &&
					!img.intersect(nearestBottomLeft))
				{
					// found a valid place
					count++;
				}
			}
		}

		cout << "Case #" << (i+1) << ": " << count << endl;
	}
}
