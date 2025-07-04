import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, Arrow
import matplotlib.patheffects as pe

# Helper to draw a professional box with consistent style
def draw_box(ax, text, xy, width, height, facecolor, edgecolor='black', fontsize=11):
    box = FancyBboxPatch(xy, width, height,
                         boxstyle="round,pad=0.02",
                         ec=edgecolor, fc=facecolor, lw=1.8,
                         path_effects=[pe.withStroke(linewidth=2, foreground="black")])
    ax.add_patch(box)
    ax.text(xy[0] + width / 2, xy[1] + height / 2, text,
            ha="center", va="center", fontsize=fontsize, weight='bold')

# Create figure and axis
fig, ax = plt.subplots(figsize=(12, 8))
ax.set_xlim(0, 10)
ax.set_ylim(0, 12)
ax.axis('off')

# Define box colors
colors = {
    "input": "#CCE5FF",          # soft blue
    "imbalance": "#D5F5E3",      # light green
    "objective": "#F9E79F",      # light yellow
    "objective4": "#F5CBA7",     # light orange
}

# Draw boxes
draw_box(ax, "Data Ingestion\n(Excel file)", xy=(1, 10), width=3, height=1.2, facecolor=colors["input"])
draw_box(ax, "Class Imbalance\nHandling (SMOTE + Weights)", xy=(6, 10), width=3.2, height=1.2, facecolor=colors["imbalance"])

draw_box(ax, "Objective 1:\nMulticlass Classification", xy=(1, 7.8), width=3.5, height=1.3, facecolor=colors["objective"])
draw_box(ax, "Objective 2:\nModel Evaluation with\n10-fold CV", xy=(6, 7.8), width=3.5, height=1.3, facecolor=colors["objective"])

draw_box(ax, "Objective 3:\nAge Effects via GAMs", xy=(1, 5.2), width=3.5, height=1.2, facecolor=colors["objective"])
draw_box(ax, "Objective 4:\nMultinomial GAM with\nAll 12 Disorders", xy=(6, 5.2), width=3.5, height=1.2, facecolor=colors["objective4"])

draw_box(ax, "Outputs:\nConfusion Matrices,\nImportance Plots,\nGAM Curves", xy=(3.5, 2.5), width=3, height=1.5, facecolor="#E8DAEF")

# Draw arrows
arrow_args = dict(arrowstyle="->", lw=1.5, color="black")
def connect(center1, center2):
    ax.annotate("", xy=center2, xytext=center1, arrowprops=arrow_args)

# Arrows from top down
connect((2.5, 10), (2.5, 9.1))  # Data -> Objective 1
connect((2.5, 9.1), (2.5, 7.8 + 1.3))  # Objective 1

connect((7.6, 10), (7.6, 9.1))  # Imbalance -> Objective 2
connect((7.6, 9.1), (7.6, 7.8 + 1.3))

connect((2.5, 7.8), (2.5, 6.4))  # Objective 1 -> Objective 3
connect((7.6, 7.8), (7.6, 6.4))  # Objective 2 -> Objective 4

connect((2.5, 5.2), (4.3, 4.0))  # Obj 3 -> Output
connect((7.6, 5.2), (5.7, 4.0))  # Obj 4 -> Output

# Save figure
plt.tight_layout()
plt.savefig("/mnt/data/workflow_overview_updated.pdf")
plt.close()

"/mnt/data/workflow_overview_updated.pdf"
