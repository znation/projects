using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;

namespace stockmarket
{
    internal class DrawingSurface
    {
        internal WriteableBitmap Bitmap { get; private set; }

        public DrawingSurface(int w, int h)
        {
            Bitmap = new WriteableBitmap(w, h, 96, 96, PixelFormats.Bgra32, null);
        }

        internal void DrawPixelUnlocked(int x, int y, Color c)
        {
            unsafe
            {
                // Get a pointer to the back buffer.
                int pBackBuffer = (int)Bitmap.BackBuffer;

                // Find the address of the pixel to draw.
                pBackBuffer += y * Bitmap.BackBufferStride;
                pBackBuffer += x * 4;

                // Compute the pixel's color.
                int color_data = c.A << 24;
                color_data |= c.R << 16;
                color_data |= c.G << 8;
                color_data |= c.B << 0;

                // Assign the color data to the pixel.
                *((int*)pBackBuffer) = color_data;
            }

            // Specify the area of the bitmap that changed.
            Bitmap.AddDirtyRect(new Int32Rect(x, y, 1, 1));
        }
    }
}
