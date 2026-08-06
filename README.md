# Quantitative Portfolio Optimization & Risk Management

A collection of quantitative finance scripts focused on portfolio optimization models, multi-asset risk scoring, and flow-driven active management strategies.


## Included Modules

🛠️ Portfolio Optimization Models
**`Black-Litterman`**: Combines market equilibrium prior distributions with investor subjective views to generate optimized asset weights and refined expected returns.
**`Minimum Variance`**: Constructs an optimal portfolio focused on minimizing total variance based on asset covariance matrices and risk constraints.
**`Quadratic Utility`**: Optimizes asset allocation by maximizing expected utility, balancing portfolio return against risk aversion parameters.


📊 Risk Assessment & Leverage
**`Portfolio Risk-Score Leverage`**: Evaluates individual asset risk contributions alongside overall portfolio weighted risk, integrating dynamic leverage adjustments.


⚡ Active Management & Tactical Flow
**`Active Management`**: Driven by options order flow mechanics to generate dynamic portfolio signals—determining whether to **hold**, **trim**, **liquidate**, or **rebalance/increase** exposures across constituent assets.


## Getting Started

Ensure you have your quantitative environment configured with the standard scientific stack (`python`, `numpy`, `pandas`, `scipy`) before running the individual scripts.
