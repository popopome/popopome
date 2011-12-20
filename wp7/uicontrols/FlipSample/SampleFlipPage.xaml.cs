using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Animation;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;
using Microsoft.Phone.Controls;

namespace FlipSample
{
    public partial class SampleFlipPage : PhoneApplicationPage
    {
        public SampleFlipPage()
        {
            InitializeComponent();

            string[] fn = new string[]
            {
                "img.jpg",
                "img01.jpg",
                "img02.jpg",
                "img03.jpg",
                "img04.jpg",
                "img05.jpg",
                "img06.jpg",
                "img07.jpg"
            };

            foreach (var f in fn)
            {
                var wbmp = BitmapFromResource(f);
                _flip.AddPageBitmap(wbmp);
            }
        }

        static WriteableBitmap BitmapFromResource(string path)
        {
            var stminfo = Application.GetResourceStream(new Uri(path, UriKind.Relative));
            var bmpimg = new BitmapImage
            {
                CreateOptions = BitmapCreateOptions.None
            };
            bmpimg.SetSource(stminfo.Stream);
            var img = new Image
            {
                Width = 480,
                Height = 800,
                Source = bmpimg,
                Stretch = Stretch.UniformToFill
            };
            var wbmp = new WriteableBitmap(img, null);
            wbmp.Invalidate();
            wbmp.Render(img, null);
            return wbmp;
        }
    }
}