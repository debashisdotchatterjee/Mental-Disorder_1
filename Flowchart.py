import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch
# Create figure
fig, ax = plt.subplots(figsize=(12, 3))
ax.axis('off')

# Box settings
boxes = [
    ("Data\nIngestion", 0.02, 0.5),
    ("Imbalance\nMitigation\nweights & SMOTE", 0.22, 0.5),
    ("Obj‑1:\nSupervised\nClassification", 0.42, 0.5),
    ("Obj‑2:\nSymptom\nStructure", 0.62, 0.5),
    ("Obj‑3:\nAge‑Effect\nModelling", 0.82, 0.5),
    ("Obj‑4:\nLatent\nClasses", 1.02, 0.5)
]
out_box = ("Outputs:\nMetrics • Factors • Curves • Classes", 0.62, 0.15)

box_w, box_h = 0.16, 0.28
for text, x, y in boxes:
    p = FancyBboxPatch((x, y), box_w, box_h, boxstyle="round,pad=0.02", linewidth=1)
    ax.add_patch(p)
    ax.text(x + box_w/2, y + box_h/2, text, ha='center', va='center', fontsize=9)

# Output box
text, x, y = out_box
p = FancyBboxPatch((x, y), 0.3, box_h, boxstyle="round,pad=0.02", linewidth=1)
ax.add_patch(p)
ax.text(x + 0.15, y + box_h/2, text, ha='center', va='center', fontsize=9)

# Arrows between main pipeline
for i in range(len(boxes)-1):
    x0 = boxes[i][1] + box_w
    y0 = boxes[i][2] + box_h/2
    x1 = boxes[i+1][1]
    y1 = boxes[i+1][2] + box_h/2
    ax.annotate('', (x1, y1), (x0, y0), arrowprops=dict(arrowstyle='->', lw=1))

# Arrows down to outputs
for idx in [2,3,4,5]:
    x0 = boxes[idx][1] + box_w/2
    y0 = boxes[idx][2]
    x1 = out_box[1] + 0.15
    y1 = out_box[2] + box_h
    ax.annotate('', (x1, y1), (x0, y0), arrowprops=dict(arrowstyle='->', lw=1))

# Save
pdf_path = "/mnt/data/workflow_overview.pdf"
fig.savefig(pdf_path, bbox_inches='tight')
plt.close(fig)
pdf_path
