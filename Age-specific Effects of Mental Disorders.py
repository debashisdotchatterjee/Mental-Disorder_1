# Refit the multinomial GAM model because the object `gam` was lost
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import PolynomialFeatures
from sklearn.pipeline import make_pipeline
from sklearn.multiclass import OneVsRestClassifier

# Redefine features and target
X = df[['age']]
y = df['Disorder']

# Polynomial features of age to approximate smooth effects (GAM-like)
model = make_pipeline(PolynomialFeatures(degree=3), OneVsRestClassifier(LogisticRegression(max_iter=10000)))
model.fit(X, y)

# Predict probabilities for each class at each age point
new_data = pd.DataFrame({'age': age_points})
pred_probs = model.predict_proba(new_data)

# Convert to long-form DataFrame for plotting
pred_df = pd.DataFrame(pred_probs, columns=model.classes_)
pred_df['age'] = age_points
pred_df_long = pred_df.melt(id_vars='age', var_name='Disorder', value_name='Probability')

# Plot using seaborn
plt.figure(figsize=(12, 8))
sns.lineplot(data=pred_df_long, x='age', y='Probability', hue='Disorder')
plt.title('Multinomial GAM (Polynomial Approximation): Age-wise Predicted Probabilities for Each Disorder')
plt.xlabel('Age')
plt.ylabel('Predicted Probability')
plt.legend(title='Disorder', bbox_to_anchor=(1.05, 1), loc='upper left')
plt.tight_layout()
# Save the final corrected figure
corrected_fig_path = "/mnt/data/FigS8_Multinomial_GAM_Age_Disorder.png"
plt.savefig(corrected_fig_path)
plt.show()
