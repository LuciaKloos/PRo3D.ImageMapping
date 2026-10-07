<#
.SYNOPSIS
    Pixel checks for the shadows/highlights test cases (see CLAUDE.md).

.EXAMPLE
    powershell -NoProfile -ExecutionPolicy Bypass -File tests\shadows-highlights\check.ps1 -Case identity `
        -InputPath tests\shadows-highlights\input\butte.png `
        -OutputPath tests\shadows-highlights\output\identity.png

    Exit code 0 = pass, 1 = fail, 2 = usage error / images not comparable.
    Values are on a 0-255 scale; only pixels with output alpha > 0 are evaluated.
    Use lossless (PNG) inputs only; JPEG decoders differ by up to ~20 levels.
#>
param(
    [Parameter(Mandatory = $true)]
    [ValidateSet('identity', 'shadows-only', 'highlights-only')]
    [string]$Case,

    [Parameter(Mandatory = $true)] [string]$InputPath,
    [Parameter(Mandatory = $true)] [string]$OutputPath,

    # identity: allowed max difference per channel
    [int]$IdentityTolerance = 1,

    # shadows-only / highlights-only: minimum change of mean luminance (0-255 scale)
    # in the dark / bright region that counts as "clearly"
    [double]$MinLuminanceChange = 2.0
)

$ErrorActionPreference = 'Stop'

Add-Type -ReferencedAssemblies System.Drawing -TypeDefinition @'
using System;
using System.Drawing;
using System.Drawing.Imaging;
using System.Runtime.InteropServices;

public class PixelStats {
    public int Width, Height;
    public long Count;                       // pixels with output alpha > 0
    public int[] MaxAbsDiff = new int[3];    // R, G, B
    public double[] MeanAbsDiff = new double[3];
    public long PixelsOverTolerance;
    public long Increased, Decreased;        // pixels with any channel increased / decreased
    public int MaxIncrease, MaxDecrease;
    public long DarkCount, BrightCount;
    public double DarkMeanIn, DarkMeanOut, BrightMeanIn, BrightMeanOut;
}

public static class ShCheck {
    // Rec. 709 weights, as in Shaders.hshColorsAdjustment
    const double WR = 0.2126, WG = 0.7152, WB = 0.0722;

    static byte[] Load(string path, out int w, out int h) {
        using (var src = new Bitmap(path))
        using (var bmp = new Bitmap(src.Width, src.Height, PixelFormat.Format32bppArgb)) {
            using (var g = Graphics.FromImage(bmp)) {
                // explicit target rectangle: DrawImageUnscaled would rescale by image DPI
                g.CompositingMode = System.Drawing.Drawing2D.CompositingMode.SourceCopy;
                g.DrawImage(src, new Rectangle(0, 0, src.Width, src.Height));
            }
            w = bmp.Width; h = bmp.Height;
            var data = bmp.LockBits(new Rectangle(0, 0, w, h), ImageLockMode.ReadOnly, PixelFormat.Format32bppArgb);
            var buf = new byte[w * h * 4];
            for (int y = 0; y < h; y++)
                Marshal.Copy(data.Scan0 + y * data.Stride, buf, y * w * 4, w * 4);
            bmp.UnlockBits(data);
            return buf; // BGRA
        }
    }

    // mode: 0 = as is, 1 = output flipped vertically, 2 = output mirrored horizontally
    public static PixelStats Compare(string inputPath, string outputPath, int mode, int tolerance) {
        int wa, ha, wb, hb;
        var A = Load(inputPath, out wa, out ha);
        var B = Load(outputPath, out wb, out hb);
        if (wa != wb || ha != hb)
            throw new Exception(String.Format("Size mismatch: input {0}x{1}, output {2}x{3}", wa, ha, wb, hb));

        var s = new PixelStats { Width = wa, Height = ha };
        double[] sum = new double[3];
        for (int y = 0; y < ha; y++)
        for (int x = 0; x < wa; x++) {
            int ia = (y * wa + x) * 4;
            int xb = mode == 2 ? wa - 1 - x : x;
            int yb = mode == 1 ? ha - 1 - y : y;
            int ib = (yb * wa + xb) * 4;
            if (B[ib + 3] == 0) continue;
            s.Count++;

            bool over = false, inc = false, dec = false;
            for (int c = 0; c < 3; c++) {          // BGRA -> c: 0 = B, 1 = G, 2 = R
                int d = B[ib + c] - A[ia + c];
                int ad = Math.Abs(d);
                int ch = 2 - c;                     // store as R, G, B
                if (ad > s.MaxAbsDiff[ch]) s.MaxAbsDiff[ch] = ad;
                sum[ch] += ad;
                if (ad > tolerance) over = true;
                if (d > 0) { inc = true; if (d > s.MaxIncrease) s.MaxIncrease = d; }
                if (d < 0) { dec = true; if (-d > s.MaxDecrease) s.MaxDecrease = -d; }
            }
            if (over) s.PixelsOverTolerance++;
            if (inc) s.Increased++;
            if (dec) s.Decreased++;

            double lumIn  = WR * A[ia + 2] + WG * A[ia + 1] + WB * A[ia];
            double lumOut = WR * B[ib + 2] + WG * B[ib + 1] + WB * B[ib];
            if (lumIn < 0.25 * 255.0) { s.DarkCount++;   s.DarkMeanIn += lumIn;   s.DarkMeanOut += lumOut; }
            if (lumIn > 0.75 * 255.0) { s.BrightCount++; s.BrightMeanIn += lumIn; s.BrightMeanOut += lumOut; }
        }
        for (int c = 0; c < 3; c++) s.MeanAbsDiff[c] = s.Count > 0 ? sum[c] / s.Count : 0.0;
        if (s.DarkCount > 0)   { s.DarkMeanIn /= s.DarkCount;     s.DarkMeanOut /= s.DarkCount; }
        if (s.BrightCount > 0) { s.BrightMeanIn /= s.BrightCount; s.BrightMeanOut /= s.BrightCount; }
        return s;
    }
}
'@

$inv = [Globalization.CultureInfo]::InvariantCulture
function Fmt([double]$v) { $v.ToString('0.000', $inv) }

foreach ($p in $InputPath, $OutputPath) {
    if (-not (Test-Path $p)) { Write-Host "File not found: $p"; exit 2 }
}
if ([IO.Path]::GetExtension($InputPath) -ieq '.jpg' -or [IO.Path]::GetExtension($InputPath) -ieq '.jpeg') {
    Write-Host 'WARNING: JPEG input; decoder differences make the identity check unreliable.'
}

try {
    $s = [ShCheck]::Compare((Resolve-Path $InputPath).Path, (Resolve-Path $OutputPath).Path, 0, $IdentityTolerance)
} catch {
    Write-Host $_.Exception.InnerException.Message
    exit 2
}

Write-Host ("{0}: {1}x{2}, evaluated pixels (alpha > 0): {3}" -f $Case, $s.Width, $s.Height, $s.Count)
Write-Host ("  max |diff| R/G/B = {0}/{1}/{2}, mean |diff| R/G/B = {3}/{4}/{5}" -f `
    $s.MaxAbsDiff[0], $s.MaxAbsDiff[1], $s.MaxAbsDiff[2],
    (Fmt $s.MeanAbsDiff[0]), (Fmt $s.MeanAbsDiff[1]), (Fmt $s.MeanAbsDiff[2]))

$failures = @()

switch ($Case) {
    'identity' {
        Write-Host ("  pixels with diff > {0}: {1}" -f $IdentityTolerance, $s.PixelsOverTolerance)
        if ($s.PixelsOverTolerance -gt 0) {
            $failures += "max difference exceeds $IdentityTolerance"
            # identity failures are usually wiring errors; check for flipped orientation
            foreach ($m in 1, 2) {
                $f = [ShCheck]::Compare((Resolve-Path $InputPath).Path, (Resolve-Path $OutputPath).Path, $m, $IdentityTolerance)
                if ($f.PixelsOverTolerance -eq 0) {
                    $failures += @('output is flipped vertically', 'output is mirrored horizontally')[$m - 1]
                }
            }
        }
    }
    'shadows-only' {
        $delta = $s.DarkMeanOut - $s.DarkMeanIn
        Write-Host ("  pixels with a decreased channel: {0} (max decrease {1})" -f $s.Decreased, $s.MaxDecrease)
        Write-Host ("  dark region (lum < 0.25, {0} px): mean luminance {1} -> {2} (delta {3})" -f `
            $s.DarkCount, (Fmt $s.DarkMeanIn), (Fmt $s.DarkMeanOut), (Fmt $delta))
        if ($s.Decreased -gt 0) { $failures += 'some channels decreased' }
        if ($s.DarkCount -eq 0) { $failures += 'no dark pixels in input' }
        elseif ($delta -lt $MinLuminanceChange) { $failures += "dark-region luminance increase < $MinLuminanceChange" }
    }
    'highlights-only' {
        $delta = $s.BrightMeanIn - $s.BrightMeanOut
        Write-Host ("  pixels with an increased channel: {0} (max increase {1})" -f $s.Increased, $s.MaxIncrease)
        Write-Host ("  bright region (lum > 0.75, {0} px): mean luminance {1} -> {2} (delta -{3})" -f `
            $s.BrightCount, (Fmt $s.BrightMeanIn), (Fmt $s.BrightMeanOut), (Fmt $delta))
        if ($s.Increased -gt 0) { $failures += 'some channels increased' }
        if ($s.BrightCount -eq 0) { $failures += 'no bright pixels in input' }
        elseif ($delta -lt $MinLuminanceChange) { $failures += "bright-region luminance decrease < $MinLuminanceChange" }
    }
}

if ($failures.Count -eq 0) {
    Write-Host 'PASS'
    exit 0
} else {
    Write-Host ('FAIL: ' + ($failures -join '; '))
    exit 1
}
