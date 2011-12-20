using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Net;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Markup;
using System.Windows.Media;
using System.Windows.Media.Animation;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;
using Microsoft.Phone.Controls;

namespace FlipSample
{
    public partial class MainPage : PhoneApplicationPage
    {
        struct FlipPage
        {
            public WriteableBitmap Front { get; set; }
            public WriteableBitmap Back { get; set; }
        }

        List<FlipPage> _pages = new List<FlipPage>();
        List<Border> _pageElements = new List<Border>();

        double _curPage = 0.0;

        #region CurrentPageNum DependencyProperty

        /// <summary>
        /// The <see cref="CurrentPageNum" /> dependency property's name.
        /// </summary>
        public const string CurrentPageNumPropertyName = "CurrentPageNum";

        /// <summary>
        /// Gets or sets the value of the <see cref="CurrentPageNum" />
        /// property. This is a dependency property.
        /// </summary>
        public double CurrentPageNum
        {
            get
            {
                return (double)GetValue(CurrentPageNumProperty);
            }
            set
            {
                SetValue(CurrentPageNumProperty, value);
            }
        }

        /// <summary>
        /// Identifies the <see cref="CurrentPageNum" /> dependency property.
        /// </summary>
        public static readonly DependencyProperty CurrentPageNumProperty = DependencyProperty.Register(
            CurrentPageNumPropertyName,
            typeof(double),
            typeof(MainPage),
            new PropertyMetadata(0.0, (x, xe) =>
                {
                    var pg = x as MainPage;
                    pg.MovePageTo((double)xe.NewValue);
                }));

        #endregion CurrentPageNum DependencyProperty

        Storyboard _sbPageMove;

        double _pageAtDown = 0.0;
        double _maxPageNumDragged = 0.0;
        double _minPageNumDragged = 0.0;
        bool _isDraggingToNext;

        List<WriteableBitmap> _samplebitmaps;

        WriteableBitmap[] _bmps;
        struct PixelData
        {
            public int[] Pixels { get; set; }
        }
        PixelData[] _pixellist;

        // Constructor
        public MainPage()
        {
            InitializeComponent();

            this.ManipulationStarted += new EventHandler<ManipulationStartedEventArgs>(_uppart_ManipulationStarted);
            this.ManipulationDelta += new EventHandler<ManipulationDeltaEventArgs>(_uppart_ManipulationDelta);
            this.ManipulationCompleted += new EventHandler<ManipulationCompletedEventArgs>(_uppart_ManipulationCompleted);

            _pageElements.Add(_border0);
            _pageElements.Add(_border2);
            _pageElements.Add(_border1);

            _pixellist = new PixelData[6];
            _bmps = new WriteableBitmap[6];
            for (int i = 0; i < _bmps.Length; ++i)
            {
                var bmp = new WriteableBitmap(480, 400);
                _bmps[i] = bmp;
                _pixellist[i].Pixels = bmp.Pixels;
            }

            _samplebitmaps = new List<WriteableBitmap>();
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
                var wbmpout0 = new WriteableBitmap(480, 400);
                var wbmpout1 = new WriteableBitmap(480, 400);
                SplitBitmap(wbmp, wbmpout0, wbmpout1);

                _samplebitmaps.Add(wbmpout0);
                _samplebitmaps.Add(wbmpout1);
            }

            var firstpage = new FlipPage
            {
                Front = _samplebitmaps[0],
                Back = _samplebitmaps[0]
            };
            _pages.Add(firstpage);

            for (int i = 1; i < _samplebitmaps.Count - 1; i += 2)
            {
                var page = new FlipPage
                {
                    Front = _samplebitmaps[i],
                    Back = _samplebitmaps[i + 1]
                };
                _pages.Add(page);
            }

            var lastpage = new FlipPage
            {
                Front = _samplebitmaps.Last(),
                Back = _samplebitmaps.Last()
            };
            _pages.Add(lastpage);

            MovePageTo(0);

            const string XAML_MOVE_ANI = @"
            <Storyboard x:Name=""_sbCurrentPage""
xmlns=""http://schemas.microsoft.com/winfx/2006/xaml/presentation""
    xmlns:x=""http://schemas.microsoft.com/winfx/2006/xaml""
>
			<DoubleAnimation Duration=""0:0:0.4"" To=""0"" Storyboard.TargetProperty=""CurrentPageNum"">
				<DoubleAnimation.EasingFunction>
					<PowerEase EasingMode=""EaseOut""/>
				</DoubleAnimation.EasingFunction>
			</DoubleAnimation>
		</Storyboard>";

            _sbPageMove = XamlReader.Load(XAML_MOVE_ANI) as Storyboard;
            Storyboard.SetTarget(_sbPageMove, this);

        }

        static void SplitBitmap(WriteableBitmap src,
                            WriteableBitmap dst1,
                            WriteableBitmap dst2)
        {
            Debug.Assert(src.PixelWidth == dst1.PixelWidth);
            Debug.Assert(src.PixelWidth == dst2.PixelWidth);
            Debug.Assert(src.PixelHeight / 2 == dst1.PixelHeight);
            Debug.Assert(src.PixelHeight / 2 == dst2.PixelHeight);

            Array.Copy(src.Pixels, dst1.Pixels, dst1.Pixels.Length);
            Array.Copy(src.Pixels, dst1.Pixels.Length, dst2.Pixels, 0, dst2.Pixels.Length);
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

        void _uppart_ManipulationStarted(object sender, ManipulationStartedEventArgs e)
        {
            var trans = e.ManipulationContainer.TransformToVisual(this);
            var touchpoint = trans.Transform(e.ManipulationOrigin);
            if (touchpoint.Y < 400)
            {
                _maxPageNumDragged = _curPage;
                _minPageNumDragged = Math.Max(0, _curPage - 1);
                _isDraggingToNext = false;
            }
            else
            {
                _maxPageNumDragged = Math.Min(_pages.Count, _curPage + 1);
                _minPageNumDragged = _curPage;
                _isDraggingToNext = true;
            }

            _pageAtDown = _curPage;
            /*_sbFlipDown.Stop();*/
        }

        void _uppart_ManipulationDelta(object sender, ManipulationDeltaEventArgs e)
        {
            var tr = e.CumulativeManipulation.Translation;
            double value = _pageAtDown;
            if (_isDraggingToNext)
            {
                value += -(tr.Y) / 800.0;
            }
            else
            {
                value -= tr.Y / 800.0;
            }

            if (value < 0)
                return;

            MovePageTo(value);
        }

        void _uppart_ManipulationCompleted(object sender, ManipulationCompletedEventArgs e)
        {
            double targetpage = 0;

            if (e.IsInertial == true
                || Math.Abs(e.FinalVelocities.LinearVelocity.Y) > 500)
            {
                //
                // Flipping happened.
                //
                if (_isDraggingToNext)
                    targetpage = Math.Floor(_curPage + 1);
                else
                    targetpage = Math.Ceiling(_curPage - 1);

                if (targetpage < 0)
                    targetpage = 0;
                else if (targetpage >= _pages.Count)
                    targetpage = _pages.Count - 1;
            }
            else
                targetpage = Math.Floor(_curPage + 0.5);

            _sbPageMove.Stop();
            var ani = _sbPageMove.Children[0] as DoubleAnimation;
            ani.From = _curPage;
            ani.To = targetpage;
            _sbPageMove.Begin();
        }

        void MovePageTo(double pagenum)
        {
            (_pageElements[0].Child as Image).Source = null;
            (_pageElements[1].Child as Image).Source = null;
            (_pageElements[2].Child as Image).Source = null;

            int curPageNum = (int)Math.Floor(pagenum);
            var curPage = _pages[curPageNum];

            var curPageEl = _pageElements[1];
            double fract = pagenum - curPageNum;

            var plane = curPageEl.Projection as PlaneProjection;
            plane.RotationX = fract * -180;

            var image = curPageEl.Child as Image;
            if (plane.RotationX > -79)
            {
                image.Source = curPage.Front;
                (image.RenderTransform as CompositeTransform).Rotation = 0;
                (image.RenderTransform as CompositeTransform).ScaleX = 1;
            }
            else
            {
                image.Source = curPage.Back;
                (image.RenderTransform as CompositeTransform).Rotation = 180;
                (image.RenderTransform as CompositeTransform).ScaleX = -1;
            }

            bool isaligned = (plane.RotationX == 0
                           || plane.RotationX == -180);
            if (isaligned == false)
                image.Opacity = 0.9;
            else
                image.Opacity = 1.0;

            int prevPageNum = curPageNum - 1;
            if (prevPageNum >= 0)
            {
                var prevPage = _pages[prevPageNum];
                var prevPageEl = _pageElements[0];

                var prevImage = prevPageEl.Child as Image;
                prevImage.Source = prevPage.Back;
                (prevImage.RenderTransform as CompositeTransform).Rotation = 180;
                (prevImage.RenderTransform as CompositeTransform).ScaleX = -1;

                var prevPlane = prevPageEl.Projection as PlaneProjection;
                prevPlane.RotationX = -180;
            }

            int nextPageNum = curPageNum + 1;
            if (nextPageNum < _pages.Count)
            {
                var nextPage = _pages[nextPageNum];
                var nextPageEl = _pageElements[2];
                var nextImage = nextPageEl.Child as Image;
                nextImage.Source = nextPage.Front;

                var nextPlane = nextPageEl.Projection as PlaneProjection;
                nextPlane.RotationX = 0;
            }

            _curPage = pagenum;
        }
    }
}