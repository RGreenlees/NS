#include "hud.h"
#include "cl_util.h"
#include "parsemsg.h"
#include "hudgl.h"

// Crosshair system based on OpenAG
// https://github.com/YaLTeR/OpenAG

int CHudCrosshairs::Init()
{
	m_iFlags = HUD_ACTIVE;

	cl_cross = CVAR_CREATE("cl_cross", "1", FCVAR_ARCHIVE);
	cl_cross_color = CVAR_CREATE("cl_cross_color", "255 255 255", FCVAR_ARCHIVE);
	cl_cross_alpha = CVAR_CREATE("cl_cross_alpha", "255", FCVAR_ARCHIVE);
	cl_cross_thickness = CVAR_CREATE("cl_cross_thickness", "2", FCVAR_ARCHIVE);
	cl_cross_size = CVAR_CREATE("cl_cross_size", "6", FCVAR_ARCHIVE);
	cl_cross_gap = CVAR_CREATE("cl_cross_gap", "3", FCVAR_ARCHIVE);
	cl_cross_outline = CVAR_CREATE("cl_cross_outline", "1", FCVAR_ARCHIVE);
	cl_cross_outline_alpha = CVAR_CREATE("cl_cross_outline_alpha", "", FCVAR_ARCHIVE);
	cl_cross_outline_inner = CVAR_CREATE("cl_cross_outline_inner", "0", FCVAR_ARCHIVE);
	cl_cross_circle_radius = CVAR_CREATE("cl_cross_circle_radius", "0", FCVAR_ARCHIVE);
	cl_cross_circle_color = CVAR_CREATE("cl_cross_circle_color", "", FCVAR_ARCHIVE);
	cl_cross_circle_thickness = CVAR_CREATE("cl_cross_circle_thickness", "1", FCVAR_ARCHIVE);
	cl_cross_circle_alpha = CVAR_CREATE("cl_cross_circle_alpha", "", FCVAR_ARCHIVE);
	cl_cross_circle_outline = CVAR_CREATE("cl_cross_circle_outline", "", FCVAR_ARCHIVE);
	cl_cross_circle_outline_alpha = CVAR_CREATE("cl_cross_circle_outline_alpha", "", FCVAR_ARCHIVE);
	cl_cross_circle_outline_inner = CVAR_CREATE("cl_cross_circle_outline_inner", "", FCVAR_ARCHIVE);
	cl_cross_dot_size = CVAR_CREATE("cl_cross_dot_size", "0", FCVAR_ARCHIVE);
	cl_cross_dot_color = CVAR_CREATE("cl_cross_dot_color", "", FCVAR_ARCHIVE);
	cl_cross_dot_alpha = CVAR_CREATE("cl_cross_dot_alpha", "", FCVAR_ARCHIVE);
	cl_cross_dot_outline = CVAR_CREATE("cl_cross_dot_outline", "0", FCVAR_ARCHIVE);
	cl_cross_dot_outline_alpha = CVAR_CREATE("cl_cross_dot_outline_alpha", "", FCVAR_ARCHIVE);
	cl_cross_line_top = CVAR_CREATE("cl_cross_line_top", "1", FCVAR_ARCHIVE);
	cl_cross_line_bottom = CVAR_CREATE("cl_cross_line_bottom", "1", FCVAR_ARCHIVE);
	cl_cross_line_left = CVAR_CREATE("cl_cross_line_left", "1", FCVAR_ARCHIVE);
	cl_cross_line_right = CVAR_CREATE("cl_cross_line_right", "1", FCVAR_ARCHIVE);

	gHUD.AddHudElem(this);
	return 0;
}

int CHudCrosshairs::VidInit()
{
	return 1;
}

int CHudCrosshairs::Draw(float time)
{
	if (cl_cross->value == 0.0f)
		return 0;

	unsigned char alpha;
	if (sscanf(cl_cross_alpha->string, "%hhu", &alpha) != 1)
		alpha = 255;

	if (alpha == 0)
		return 0;

	if (gHUD.GetInTopDownMode())
		return 0;

	unsigned char r, g, b;
	if (sscanf(cl_cross_color->string, "%hhu %hhu %hhu", &r, &g, &b) != 3) {
		r = 255;
		g = 255;
		b = 255;
	}

	unsigned char outalpha;
	if (sscanf(cl_cross_outline_alpha->string, "%hhu", &outalpha) != 1)
		outalpha = alpha;


	Vector2D center(ScreenWidth() / 2.0f, ScreenHeight() / 2.0f);

	HudGL gl;

	// Draw the outline.
	// TODO: this contains a terrible amount of repeating complex code.
	//
	// Possible solution: can be changed to this with the one downside being in rare cases where cl_cross_thickness is high AND its alpha is <255, the center of each line will be slightly darker.
	// Example below is for bottom line. Would also cause certain crosshairs that have outlines on invisible cross lines to not look right.
	// gl.rectangle(Vector2D(center.x + offset, center.y + gap - half_width), Vector2D(center.x - offset, center.y + gap + size + half_width));
	if (cl_cross_outline->value > 0.0f && cl_cross_thickness->value > 0.0f && cl_cross_size->value > 0.0f) {


		float size = cl_cross_size->value;
		float gap = cl_cross_gap->value;
		float half_thickness = min(cl_cross_thickness->value, ScreenHeight() * 0.3f) * 0.5f;
		float outline_width = cl_cross_outline->value;
		float offset = half_thickness + outline_width;

		gl.color(0, 0, 0, outalpha);
		gl.line_width(outline_width * 2.0f);

		// Top line
		if (cl_cross_line_top->value) {
			gl.line(Vector2D(center.x - offset, center.y - gap - size), Vector2D(center.x + offset, center.y - gap - size));
			if (cl_cross_outline_inner->value == 0.0f)
			{
				gl.line(Vector2D(center.x + half_thickness, center.y - gap - size + outline_width), Vector2D(center.x + half_thickness, center.y - gap));
				gl.line(Vector2D(center.x - half_thickness, center.y - gap), Vector2D(center.x - half_thickness, center.y - gap - size + outline_width));
			}
			else
			{
				gl.line(Vector2D(center.x + half_thickness, center.y - gap - size + outline_width), Vector2D(center.x + half_thickness, center.y - gap - outline_width));
				gl.line(Vector2D(center.x + offset, center.y - gap), Vector2D(center.x - offset, center.y - gap));
				gl.line(Vector2D(center.x - half_thickness, center.y - gap - outline_width), Vector2D(center.x - half_thickness, center.y - gap - size + outline_width));
			}
		}

		// Bottom line
		if (cl_cross_line_bottom->value) {
			gl.line(Vector2D(center.x - offset, center.y + gap + size), Vector2D(center.x + offset, center.y + gap + size));
			if (cl_cross_outline_inner->value == 0.0f)
			{
				gl.line(Vector2D(center.x + half_thickness, center.y + gap + size - outline_width), Vector2D(center.x + half_thickness, center.y + gap));
				gl.line(Vector2D(center.x - half_thickness, center.y + gap), Vector2D(center.x - half_thickness, center.y + gap + size - outline_width));
			}
			else
			{
				gl.line(Vector2D(center.x + half_thickness, center.y + gap + size - outline_width), Vector2D(center.x + half_thickness, center.y + gap + outline_width));
				gl.line(Vector2D(center.x + offset, center.y + gap), Vector2D(center.x - offset, center.y + gap));
				gl.line(Vector2D(center.x - half_thickness, center.y + gap + outline_width), Vector2D(center.x - half_thickness, center.y + gap + size - outline_width));
			}
		}

		// Left line
		if (cl_cross_line_left->value) {
			gl.line(Vector2D(center.x - gap - size, center.y - offset), Vector2D(center.x - gap - size, center.y + offset));
			if (cl_cross_outline_inner->value == 0.0f)
			{
				gl.line(Vector2D(center.x - gap - size + outline_width, center.y + half_thickness), Vector2D(center.x - gap, center.y + half_thickness));
				gl.line(Vector2D(center.x - gap, center.y - half_thickness), Vector2D(center.x - gap - size + outline_width, center.y - half_thickness));
			}
			else
			{
				gl.line(Vector2D(center.x - gap - size + outline_width, center.y + half_thickness), Vector2D(center.x - gap - outline_width, center.y + half_thickness));
				gl.line(Vector2D(center.x - gap, center.y + offset), Vector2D(center.x - gap, center.y - offset));
				gl.line(Vector2D(center.x - gap - outline_width, center.y - half_thickness), Vector2D(center.x - gap - size + outline_width, center.y - half_thickness));
			}
		}

		// Right line
		if (cl_cross_line_right->value) {
			gl.line(Vector2D(center.x + gap + size, center.y - offset), Vector2D(center.x + gap + size, center.y + offset));
			if (cl_cross_outline_inner->value == 0.0f)
			{
				gl.line(Vector2D(center.x + gap + size - outline_width, center.y + half_thickness), Vector2D(center.x + gap, center.y + half_thickness));
				gl.line(Vector2D(center.x + gap, center.y - half_thickness), Vector2D(center.x + gap + size - outline_width, center.y - half_thickness));
			}
			else
			{
				gl.line(Vector2D(center.x + gap + size - outline_width, center.y + half_thickness), Vector2D(center.x + gap + outline_width, center.y + half_thickness));
				gl.line(Vector2D(center.x + gap, center.y + offset), Vector2D(center.x + gap, center.y - offset));
				gl.line(Vector2D(center.x + gap + outline_width, center.y - half_thickness), Vector2D(center.x + gap + size - outline_width, center.y - half_thickness));
			}
		}
	}

	float dotout;

	if (cl_cross_dot_outline->string[0] == 0)
	{
		dotout = cl_cross_outline->value;
	}
	else
	{
		dotout = cl_cross_dot_outline->value;
	}
	// Dot outline
	if (cl_cross_dot_size->value > 0.0f && dotout > 0.0f) {
		unsigned char dotoutalpha;
		if (sscanf(cl_cross_dot_outline_alpha->string, "%hhu", &dotoutalpha) != 1)
			dotoutalpha = outalpha;

		gl.color(0, 0, 0, dotoutalpha);

		float size = min(cl_cross_dot_size->value, ScreenHeight() * 0.2f) + (dotout * 2.0f);
		Vector2D offset = Vector2D(size / 2.0f, size / 2.0f);

		gl.rectangle(center - offset, center + offset);
	}

	float circleout;
	if (cl_cross_circle_outline->string[0] == 0)
	{
		circleout = cl_cross_outline->value;
	}
	else
	{
		circleout = cl_cross_circle_outline->value;
	}
	// Circle outline
	if (cl_cross_circle_radius->value > 0.0f && cl_cross_circle_thickness->value > 0.0f && circleout > 0.0f) {

		unsigned char circleoutalpha;
		if (sscanf(cl_cross_circle_outline_alpha->string, "%hhu", &circleoutalpha) != 1)
			circleoutalpha = outalpha;

		bool circleoutinner = cl_cross_outline_inner->value;

		gl.color(0, 0, 0, circleoutalpha);

		auto radius = cl_cross_circle_radius->value;

		if (!circleoutinner)
		{
			radius += cl_cross_circle_thickness->value * 0.5f;
			gl.line_width(circleout);
		}
		else
		{
			gl.line_width(cl_cross_circle_thickness->value + (circleout * 2.0f));
		}

		if (old_circle_radius != radius) {
			// Recompute the circle points.
			circle_points = HudGL::compute_circle(radius);
			old_circle_radius = radius;
		}
		gl.circle(center, circle_points);
	}

	gl.color(r, g, b, alpha);

	// Draw the crosshairs.
	if (cl_cross_thickness->value > 0.0f && cl_cross_size->value > 0.0f) {
		
		float size;
		float gap = cl_cross_gap->value;

		// Box crosshair. This is needed since line thickness seems to cap out at 10 on my system.
		if (cl_cross_thickness->value > cl_cross_size->value) {
			gl.line_width(cl_cross_size->value);
			float half_size = cl_cross_size->value * 0.5f;
			float half_thickness = cl_cross_thickness->value * 0.5f;

			if (cl_cross_line_top->value)
				gl.line(Vector2D(center.x - half_thickness, center.y - gap - half_size), Vector2D(center.x + half_thickness, center.y - gap - half_size));
			if (cl_cross_line_bottom->value)
				gl.line(Vector2D(center.x - half_thickness, center.y + gap + half_size), Vector2D(center.x + half_thickness, center.y + gap + half_size));
			if (cl_cross_line_left->value)
				gl.line(Vector2D(center.x - gap - half_size, center.y + half_thickness), Vector2D(center.x - gap - half_size, center.y - half_thickness));
			if (cl_cross_line_right->value)
				gl.line(Vector2D(center.x + gap + half_size, center.y + half_thickness), Vector2D(center.x + gap + half_size, center.y - half_thickness));
		}
		// Normal cross.
		else {
			gl.line_width(cl_cross_thickness->value);
			size = cl_cross_size->value;

			if (cl_cross_line_top->value)
				gl.line(Vector2D(center.x, center.y - gap - size), Vector2D(center.x, center.y - gap));
			if (cl_cross_line_bottom->value)
				gl.line(Vector2D(center.x, center.y + gap + size), Vector2D(center.x, center.y + gap));
			if (cl_cross_line_left->value)
				gl.line(Vector2D(center.x - gap - size, center.y), Vector2D(center.x - gap, center.y));
			if (cl_cross_line_right->value)
				gl.line(Vector2D(center.x + gap + size, center.y), Vector2D(center.x + gap, center.y));
		}

	}

	// Draw the circle.
	if (cl_cross_circle_radius->value > 0.0f && cl_cross_circle_thickness->value > 0.0f) {
		unsigned char circlealpha;
		if (sscanf(cl_cross_circle_alpha->string, "%hhu", &circlealpha) != 1)
			circlealpha = alpha;

		unsigned char circr, circg, circb;
		if (sscanf(cl_cross_circle_color->string, "%hhu %hhu %hhu", &circr, &circg, &circb) == 3)
		{
			gl.color(circr, circg, circb, circlealpha);
		}
		else
		{
			gl.color(r, g, b, circlealpha);
		}
		
		gl.line_width(cl_cross_circle_thickness->value);

		float radius = cl_cross_circle_radius->value;
		if (old_circle_radius != radius) {
			// Recompute the circle points.
			circle_points = HudGL::compute_circle(radius);
			old_circle_radius = radius;
		}

		gl.circle(center, circle_points);
	}

	// Draw the dot.
	if (cl_cross_dot_size->value > 0.0f) {
		unsigned char dotalpha;
		if (sscanf(cl_cross_dot_alpha->string, "%hhu", &dotalpha) != 1)
			dotalpha = alpha;

		unsigned char dotr, dotg, dotb;
		if (sscanf(cl_cross_dot_color->string, "%hhu %hhu %hhu", &dotr, &dotg, &dotb) == 3)
		{
			gl.color(dotr, dotg, dotb, dotalpha);
		}
		else
		{
			gl.color(r, g, b, dotalpha);
		}

		//float size = cl_cross_dot_size->value;
		//clamp dot size to prevent using it as a full screen transparent mask for highlighting eneimies like with the /nvg night vision server plugin.
		float size = min(cl_cross_dot_size->value, ScreenHeight() * 0.2f);
		Vector2D offset = Vector2D(size / 2.0f, size / 2.0f);

		gl.rectangle(center - offset, center + offset);
	}

	return 0;
}
