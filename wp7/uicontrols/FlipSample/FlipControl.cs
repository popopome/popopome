using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Linq;
using System.Net;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Ink;
using System.Windows.Input;
using System.Windows.Markup;
using System.Windows.Media;
using System.Windows.Media.Animation;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;

namespace FlipSample
{
    /// <summary>
    /// This control is for Flipboard's style paging control.
    /// </summary>
    public class FlipControl : Grid
    {
        /// <summary>
        /// Flip page declaration
        /// </summary>
        private class FlipPage
        {
            public WriteableBitmap Front { get; set; }
            public WriteableBitmap Back { get; set; }
        }

        #region Constants

        const double DESIRE_WIDTH = 480;
        const double DESIRE_HEIGHT = 800;
        const double HALF_HEIGHT = DESIRE_HEIGHT / 2;
        const double PAGE_UI_ELEMENT_WIDTH = DESIRE_WIDTH;
        const double PAGE_UI_ELEMENT_HEIGHT = HALF_HEIGHT;

        #endregion Constants

        #region Fields

        double _curPage = 0.0;
        double _pageAtDown = 0.0;
        double _maxPageNumDragged = 0.0;
        double _minPageNumDragged = 0.0;
        bool _isDraggingToNext;

        Border _prevPageUi;
        Border _nextPageUi;
        Border _curPageUi;

        ObservableCollection<FlipPage> _pages;

        Storyboard _sbPageFlip;

        WriteableBitmap _halfBackgroundBitmap;

        #endregion Fields

        #region Dependency Properties

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
            typeof(FlipControl),
            new PropertyMetadata(double.MinValue, (x, xe) =>
            {
                var pg = x as FlipControl;
                pg.MovePageTo((double)xe.NewValue);
            }));

        #endregion CurrentPageNum DependencyProperty

        #region TotalNumPages DependencyProperty

        /// <summary>
        /// The <see cref="TotalNumPages" /> dependency property's name.
        /// </summary>
        public const string TotalNumPagesPropertyName = "TotalNumPages";

        /// <summary>
        /// Gets or sets the value of the <see cref="TotalNumPages" />
        /// property. This is a dependency property.
        /// </summary>
        public int TotalNumPages
        {
            get
            {
                return (int)GetValue(TotalNumPagesProperty);
            }
            set
            {
                SetValue(TotalNumPagesProperty, value);
            }
        }

        /// <summary>
        /// Identifies the <see cref="TotalNumPages" /> dependency property.
        /// </summary>
        public static readonly DependencyProperty TotalNumPagesProperty = DependencyProperty.Register(
            TotalNumPagesPropertyName,
            typeof(int),
            typeof(FlipControl),
            new PropertyMetadata(0,
                        (x, xe) =>
                        {
                            int numpages = (int)xe.NewValue;
                            if (numpages <= 0)
                                return;

                            var c = x as FlipControl;
                            Debug.Assert(c != null);

                            if (numpages < c._pages.Count)
                            {
                                while (numpages < c._pages.Count)
                                    c._pages.RemoveAt(c._pages.Count - 1);
                            }
                            else if (numpages > c._pages.Count)
                            {
                                while (numpages > c._pages.Count)
                                {
                                    var pg = new FlipPage
                                    {
                                        Front = _halfBackgroundBitmap,
                                        Back = _halfBackgroundBitmap
                                    };

                                    c._pages.Add(pg);
                                }
                            }
                        }));

        #endregion TotalNumPages DependencyProperty

        #endregion Dependency Properties

        /// <summary>
        /// CTOR
        /// </summary>
        public FlipControl()
        {
            this.Width = DESIRE_WIDTH;
            this.Height = DESIRE_HEIGHT;

            this.ManipulationStarted += new EventHandler<ManipulationStartedEventArgs>(OnManipStarted);
            this.ManipulationDelta += new EventHandler<ManipulationDeltaEventArgs>(OnManipDelta);
            this.ManipulationCompleted += new EventHandler<ManipulationCompletedEventArgs>(OnManipCompleted);

            _prevPageUi = CreatePageUIElement();
            _nextPageUi = CreatePageUIElement();
            _curPageUi = CreatePageUIElement();

            this.Children.Add(_prevPageUi);
            this.Children.Add(_nextPageUi);
            this.Children.Add(_curPageUi);

            _pages = new ObservableCollection<FlipPage>();

            InitializePageFlipStoryboard();

            _halfBackgroundBitmap =
                new WriteableBitmap((int)PAGE_UI_ELEMENT_WIDTH,
                                    (int)PAGE_UI_ELEMENT_HEIGHT);
            _halfBackgroundBitmap.Pixels
        }

        /// <summary>
        /// Initialize page-flip storyboard
        /// </summary>
        void InitializePageFlipStoryboard()
        {
            #region XAML_MOVE_ANI

            const string XAML_MOVE_ANI = @"
            <Storyboard
                xmlns=""http://schemas.microsoft.com/winfx/2006/xaml/presentation""
                xmlns:x=""http://schemas.microsoft.com/winfx/2006/xaml""
            >
			<DoubleAnimation Duration=""0:0:0.4"" To=""0"" Storyboard.TargetProperty=""CurrentPageNum"">
				<DoubleAnimation.EasingFunction>
					<PowerEase EasingMode=""EaseOut""/>
				</DoubleAnimation.EasingFunction>
			</DoubleAnimation>
		</Storyboard>";

            #endregion XAML_MOVE_ANI

            _sbPageFlip = XamlReader.Load(XAML_MOVE_ANI) as Storyboard;
            Storyboard.SetTarget(_sbPageFlip, this);
        }

        /// <summary>
        /// Touch started
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        void OnManipStarted(object sender, ManipulationStartedEventArgs e)
        {
            var trans = e.ManipulationContainer.TransformToVisual(this);
            var touchpoint = trans.Transform(e.ManipulationOrigin);
            if (touchpoint.Y < 400)
            {
                _maxPageNumDragged = _curPage;
                _minPageNumDragged = Math.Max(1, Math.Floor(_curPage - 1));
                _isDraggingToNext = false;
            }
            else
            {
                _maxPageNumDragged = Math.Min(_pages.Count - 1, Math.Ceiling(_curPage + 1));
                _minPageNumDragged = _curPage;
                _isDraggingToNext = true;
            }

            _pageAtDown = _curPage;
        }

        /// <summary>
        /// Manipulation delta
        /// </summary>
        /// <param name="sender">Event sender</param>
        /// <param name="e">Event parameter</param>
        void OnManipDelta(object sender, ManipulationDeltaEventArgs e)
        {
            var tr = e.CumulativeManipulation.Translation;
            double value = _pageAtDown;
            if (_isDraggingToNext)
                value += -(tr.Y) / 800.0;
            else
                value -= tr.Y / 800.0;

            //
            // page 0 should not be displayed.
            // page 0 always displays half-size bitmap.
            //
            if (value <= 0.5)
                return;
            if (value >= (_pages.Count - 1 + 0.5))
                return;

            MovePageTo(value);
        }

        /// <summary>
        /// Manipulation completed
        /// </summary>
        /// <param name="sender">Event sender</param>
        /// <param name="e">Event parameter</param>
        void OnManipCompleted(
                object sender,
                ManipulationCompletedEventArgs e)
        {
            double targetpage = 0;

            if (e.IsInertial == true
                || Math.Abs(e.FinalVelocities.LinearVelocity.Y) > 500)
            {
                //
                // Flipping happened.
                //
                bool isToNext = Math.Sign(e.FinalVelocities.LinearVelocity.Y) < 0;
                if (isToNext)
                    targetpage = Math.Floor(_curPage + 1);
                else
                    targetpage = Math.Ceiling(_curPage - 1);

                //
                // page-0 should not be target page.
                // Because page-0 always displays half-size bitmap.
                //
                if (targetpage < 1)
                    targetpage = 1;
                else if (targetpage > _pages.Count - 1)
                    targetpage = _pages.Count - 1;
            }
            else
            {
                //
                // Move to nearest page
                //
                targetpage = Math.Floor(_curPage + 0.5);
            }

            if (false == IsValidPageNum(targetpage))
                return;

            _sbPageFlip.Stop();
            var ani = _sbPageFlip.Children[0] as DoubleAnimation;
            ani.From = _curPage;
            ani.To = targetpage;
            _sbPageFlip.Begin();
        }

        /// <summary>
        /// Move page to given page
        /// </summary>
        /// <param name="pagenum">
        /// Page number in double decision number.
        /// Fraction part indicates the amount of flipping state
        /// </param>
        void MovePageTo(double pagenum)
        {
            if (false == IsValidPageNum(pagenum))
            {
                Debug.WriteLine("MovePageTo called but pagenum is invalid:pagenum={0}",
                                pagenum);
                return;
            }

            (_prevPageUi.Child as Image).Source = null;
            (_curPageUi.Child as Image).Source = null;
            (_nextPageUi.Child as Image).Source = null;

            int curPageNum = (int)Math.Floor(pagenum);
            var curPage = _pages[curPageNum];

            var curPageEl = _curPageUi;
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
                var prevPageEl = _prevPageUi;

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
                var nextPageEl = _nextPageUi;
                var nextImage = nextPageEl.Child as Image;
                nextImage.Source = nextPage.Front;

                var nextPlane = nextPageEl.Projection as PlaneProjection;
                nextPlane.RotationX = 0;
            }

            _curPage = pagenum;
        }

        /// <summary>
        /// Check given pagenum is valid one or not
        /// </summary>
        /// <param name="pagenum">Page number</param>
        /// <returns>
        /// true if pagenum is valid.
        /// Otherwise return false.
        /// </returns>
        bool IsValidPageNum(double pagenum)
        {
            return pagenum >= 0
                && pagenum < _pages.Count;
        }

        /// <summary>
        /// Create page ui element
        /// </summary>
        /// <returns></returns>
        Border CreatePageUIElement()
        {
            const double ROTATION_CENTER_FOR_SEAMLESS_FLIP = 0.0005;
            var border = new Border
            {
                Width = PAGE_UI_ELEMENT_WIDTH,
                Height = PAGE_UI_ELEMENT_HEIGHT,
                VerticalAlignment = VerticalAlignment.Bottom,
                IsHitTestVisible = false,
                Background = new SolidColorBrush(Colors.DarkGray),
                CacheMode = new BitmapCache(),
                Projection = new PlaneProjection
                {
                    CenterOfRotationY = ROTATION_CENTER_FOR_SEAMLESS_FLIP
                }
            };

            var image = new Image
            {
                Stretch = Stretch.Fill,
                RenderTransformOrigin = new Point(0.5, 0.5),
                Width = PAGE_UI_ELEMENT_WIDTH,
                Height = PAGE_UI_ELEMENT_HEIGHT,
                RenderTransform = new CompositeTransform()
            };

            border.Child = image;
            return border;
        }

        /// <summary>
        /// Add page
        /// </summary>
        /// <param name="bmp">Bitmap which represents current page</param>
        public void AddPageBitmap(BitmapSource bmp)
        {
            bool wasEmpty = _pages.Count == 0;

            var wbmp = EnsureWriteableBitmap(bmp);

            var dstbmp0 = new WriteableBitmap(480, 400);
            var dstbmp1 = new WriteableBitmap(480, 400);

            SplitBitmap(wbmp, dstbmp0, dstbmp1);
            AppendFlipPage(dstbmp0, dstbmp1);

            if (wasEmpty)
            {
                //
                // Minimum number of pages are two
                // after initial page is added.
                //
                // Page number 0 displays half-visible page,
                // so the initial page should be 1
                // in order to display full-size bitmap.
                //
                CurrentPageNum = 1;
            }
        }

        /// <summary>
        /// Append flip page
        /// </summary>
        /// <param name="backOfLast"></param>
        /// <param name="frontOfNew"></param>
        void AppendFlipPage(WriteableBitmap backOfLast,
                            WriteableBitmap frontOfNew)
        {
            if (_pages.Count == 0)
            {
                var firstpage = new FlipPage { Front = backOfLast, Back = backOfLast };
                var lastpage = new FlipPage { Front = frontOfNew, Back = frontOfNew };
                _pages.Add(firstpage);
                _pages.Add(lastpage);
            }
            else
            {
                var lastpage = _pages.Last();
                lastpage.Back = backOfLast;

                var newpage = new FlipPage { Front = frontOfNew, Back = frontOfNew };
                _pages.Add(newpage);
            }
        }

        /// <summary>
        /// Ensure given bitmap is WriteableBitmap.
        /// If the bitmap is not the writeable one,
        /// create WriteableBitmap with given BitmapSource object
        /// </summary>
        /// <param name="bmp">BitmapSource object</param>
        /// <returns>WriteableBitmap object</returns>
        WriteableBitmap EnsureWriteableBitmap(BitmapSource bmp)
        {
            if (bmp is WriteableBitmap)
                return bmp as WriteableBitmap;

            return new WriteableBitmap(bmp);
        }

        /// <summary>
        /// Split bitmap with horizontal splits
        /// </summary>
        /// <param name="src">Source bitmap</param>
        /// <param name="dst1">First part of bitmap</param>
        /// <param name="dst2">Second part of bitmap</param>
        void SplitBitmap(
                    WriteableBitmap src,
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

    }
}