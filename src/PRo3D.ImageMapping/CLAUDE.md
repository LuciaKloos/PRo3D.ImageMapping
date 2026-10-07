# PRo3D.ImageMapping – notes for Claude

F# / .NET application built on the Aardvark platform (Aardvark.Base, Aardvark.Rendering,
Aardvark.Media). It displays planetary images (plain RGB, greyscale, multispectral) and
applies image adjustments, partly on the GPU. The UI runs as a local web app inside an
Aardium window.

Aardvark sources, when API details are unclear:
- https://github.com/aardvark-platform/aardvark.base
- https://github.com/aardvark-platform/aardvark.rendering
- https://github.com/aardvark-platform/aardvark.media

## Build and run

```
cd src/PRo3D.ImageMapping
dotnet build
```

Normal start (SPICE kernel path is machine-specific, ask if unknown):

```
dotnet run -- --spice "<path-to>\hera_ops.tm"
```

## Current focus: shadows/highlights adjustment

### Pipeline (all GPU, plain RGB images in the RGB category only)

1. `Image.createShadowsHighlightsMaskTexture` renders `Shaders.hshShadowsHighlights`
   offscreen: R = shadow mask, G = highlight mask (driven by the Tone sliders).
2. `Image.horizontalBlurPass` and `Image.verticalBlurPass` run `Shaders.boxBlur`
   (driven by the Radius sliders).
3. `Shaders.hshColorsAdjustment` samples the blurred mask and applies the Amount sliders
   via `Shaders.applyShadowsHighlights` (highlights: c^2.8, shadows: 1-(1-c)^2.8),
   followed by midtone contrast, saturation and brightness.
4. `Image.createAdjustedImageTexture` renders step 3 offscreen; `Image.saveAdjustedImage`
   downloads and saves it.

The CPU source texture (image load, black/white clip, midtone mask) is built in
`RgbComposite.createPlainRgbPixImageFromPath`. The CPU shadows/highlights functions in
`RgbComposite.fs` (`createShadowsHighlightsMask`, `boxBlurMask`, `applyAdjustments`) are
legacy and unused.

### Key files

| File | Relevant contents |
|---|---|
| `ImageShader.fs` | `hshShadowsHighlights`, `boxBlur`, `applyShadowsHighlights`, `hshColorsAdjustment` |
| `Image.fs` | offscreen passes, `createInstrumentScene` (display scene) |
| `App.fs` | `initialFor` (test-mode model), `shadowsHighlightsGpuSettings`, `useGpu` |
| `Program.fs` | command-line parsing, `TestConfig` |
| `Model.fs` | slider defaults (`HighlightAdjustment.init`, `ShadowAdjustment.init`) |

`shadowsHighlightsGpuSettings` is a tuple in this order:
`(highlightAmount, highlightTone, highlightRadius, shadowAmount, shadowTone, shadowRadius)`.

## Testing the shadows/highlights feature

### Test data

- Input image: `tests/shadows-highlights/input/butte.png`
- Parameter sets: `tests/shadows-highlights/cases.json` (one entry per case, with an
  `expect` field describing the intended result)
- Reference images: `tests/shadows-highlights/reference/<case-name>.png`
- Test output: `tests/shadows-highlights/output/<case-name>.png` (not committed)

### Run one case

```
dotnet run -- --spice "<path-to>\hera_ops.tm" ^
  --test-image "tests/shadows-highlights/input/butte.png" ^
  --shadow-amount 0.8 --highlight-amount 0.0 ^
  --output "tests/shadows-highlights/output/shadows-only.png"
```

Available options (defaults in brackets): `--highlight-amount` [0.0],
`--highlight-tone` [0.5], `--highlight-radius` [30], `--shadow-amount` [0.0],
`--shadow-tone` [0.5], `--shadow-radius` [30], `--output` [none].
Numbers use a dot as decimal separator.

### Checks every change must pass

Measurable (values on a 0–255 scale, alpha > 0 pixels only):

1. **identity**: output equals input, max difference ≤ 1 per channel. Failing this
   almost always means a wiring error (unbound texture, wrong sampler name, flipped
   orientation), not a math error.
2. **shadows-only**: no channel of any pixel decreases; mean luminance of pixels with
   input luminance < 0.25 increases clearly.
3. **highlights-only**: no channel of any pixel increases; mean luminance of pixels with
   input luminance > 0.75 decreases clearly.
4. not yet in use, skip **all cases vs. reference**: mean absolute difference ≤ 0.5, max difference ≤ 3.

Visual (inspect the output PNG):

- No visible halos or bright/dark rims along strong edges (e.g. sky/rock boundary).
- Mask transitions are smooth, no banding or blocky steps at large radii.
- Image is not mirrored or flipped compared to the input.

### Rules

- **Never overwrite or regenerate reference images** unless explicitly asked. If a change
  is intended to alter the look, report the differences and wait for approval.
- Save test output as PNG, never JPEG (JPEG compression breaks pixel comparisons).
- After changing a shader or pass, run at least `identity`, `shadows-only` and
  `highlights-only` before reporting success.

## Known pitfalls in this codebase

- **FShade helper functions** called from a shader need `[<ReflectedDefinition>]`,
  otherwise rendering fails with "cannot call function ... since it is not reflectable".
  Pass uniforms in as arguments rather than reading `uniform.*` inside helpers.
- **Texture names must match**: a sampler declared with `texture uniform?Foo` only finds a
  texture bound with `Sg.texture "Foo"`. A mismatch gives "[GL] Could not find texture".
- **Two `Sg` modules**: `Sg` in `Image.fs` is Aardvark.UI's (`ISg<'msg>`, for the display
  scene). Offscreen passes use the core scene graph via `CoreSg`
  (`Aardvark.SceneGraph.SgFSharp.Sg`). `open Aardvark.SceneGraph` must stay **above**
  `open Aardvark.UI`, otherwise UI scene graph code breaks.
- **No side effects in `createInstrumentScene`**: it builds a scene description and is also
  called by `view2DRelative` (thumbnail). Saving files there can run at unexpected times,
  get overwritten by the thumbnail call, and an exception breaks the whole display.
- **Windows paths in F#** must be verbatim strings (`@"C:\..."`); in normal strings `\r`,
  `\a` etc. are escape sequences.
- **GPU path conditions**: shadows/highlights only take effect for plain RGB images with
  `activeCategory = RgbImage`. Multispectral and greyscale paths bypass `adjustmentSg`.
- **Files starting with `._`** are macOS metadata files, not images.
