package bartMachine;

import java.io.Serializable;

/**
 * This class handles the parallelization of many Gibbs chains over many CPU cores
 * to create one BART regression model. It also handles all operations on the completed model.
 * @author Adam Kapelner and Justin Bleich
 * 
 */
public class bartMachineClassificationMultThread extends bartMachineRegressionMultThread implements Serializable{
	
	/** The default value of the <code>classification_rule</code> */
	private static double DEFAULT_CLASSIFICATION_RULE = 0.5;
	/** The value of the classification rule which if the probability estimate of Y = 1 is greater than, we predict 1 */
	private double classification_rule;

	/** Set up an array of binary classification BARTs with length equal to <code>num_cores</code>, the number of CPU cores requested */
	protected void SetupBARTModels() {
		bart_gibbs_chain_threads = new bartMachineClassification[num_cores];
		for (int t = 0; t < num_cores; t++){
			SetupBartModel(new bartMachineClassification(), t);
		}
		classification_rule = DEFAULT_CLASSIFICATION_RULE;
	}
	
	/**
	 * Predicts the best guess of the class for an observation
	 * 
	 * @param record				The record who's class we wish to predict
	 * @param num_cores_evaluate	The number of CPU cores to use during this operation
	 * @return						The best guess of the class based on the probability estimate evaluated against the {@link classification_rule}
	 */
	public double Evaluate(double[] record, int num_cores_evaluate) {
		return EvaluateViaSampAvg(record, num_cores_evaluate) > classification_rule ? 1 : 0;
	}	
	
	/**
	 * This returns the Gibbs sample predictions for all trees and all posterior samples.
	 * This differs from the parent implementation because we convert the response value to
	 * a probability estimate using the normal CDF.
	 * 
	 *  @param data					The data for which to generate predictions
	 *  @param num_cores_evaluate	The number of CPU cores to use during this operation
	 *  @return						The predictions as a vector of size number of posterior samples of vectors of size number of trees
	 */
	protected double[][] getGibbsSamplesForPrediction(double[][] data, int num_cores_evaluate){
		double[][] y_gibbs_samples = super.getGibbsSamplesForPrediction(data, num_cores_evaluate);
		double[][] y_gibbs_samples_probs = new double[y_gibbs_samples.length][y_gibbs_samples[0].length];
		for (int g = 0; g < y_gibbs_samples.length; g++){
			for (int i = 0; i < y_gibbs_samples[0].length; i++){
				y_gibbs_samples_probs[g][i] = StatToolbox.normal_cdf(y_gibbs_samples[g][i]);
			}			
		}
		return y_gibbs_samples_probs;
	}	
	
	public void setClassificationRule(double classification_rule) {
		this.classification_rule = classification_rule;
	}	
    // TONY'S EXTENSIONS

    // TONY prior_cov_spec additions:
    // TONY TODO : alpha_up needs a size
    // TONY TODO : Need to initialize both of these guys; this should be done elsewhere?
    protected double[][] gibbs_samples_cov_split_prior;
    protected double[] alpha_up;
    protected double alpha_0 = 1.0;
    protected int do_ard;

    public void setARD(int do_ard) {
        System.out.println("Using Automatic Relevance Detection");
        this.do_ard = do_ard;
    }

    public void setAlpha0(double alpha_0){
        System.out.println("Initialize alpha_0");
        this.alpha_0 = alpha_0;
    }

    public void initAlphaUp(int p) {
        System.out.println("Initialize alpha");
        alpha_up = new double[p];
    }

    public void initGibbsSamplesCovSplit(int num_samples, int p) {
        gibbs_samples_cov_split_prior = new double[num_samples][p]; 
    }

    // TONY

    ////////////////////////////////////////////////////////////////////////////
    // Basic Idea: Call sample_dirichlet to update, then save the result. See //
    // how draw sigsq function works for recording stuff; may call another    //
    // function to do the actual update of the state, and just store.         //
    ////////////////////////////////////////////////////////////////////////////

    // TONY : Need to add argument to updateAlpha()
    protected void updateCovSplit(int sample_num, bartMachineTreeNode[] bart_trees) {
        if(do_ard == 1) {
            System.out.println("Updating cov_split");
            updateAlpha(bart_trees);
            cov_split_prior = StatToolbox.sample_from_dirichlet(alpha_up);
            gibbs_samples_cov_split_prior[sample_num] = cov_split_prior.clone();
        }
    }

    // TONY : Define updateAlpha() 
    // TONY : To implement this, I need to get the variable counts.
    protected void updateAlpha(bartMachineTreeNode[] bart_trees) {
        int[] counts = new int[p]; 
        for(bartMachineTreeNode tree : bart_trees) {
            counts = Tools.add_arrays(counts, tree.attributeSplitCounts());
        }
        for(int i = 0; i < p; i++) {
            alpha_up[i] = alpha_0 / (double)p + (double)counts[i]; // TONY TODO : Replace 1.0 with something else
        }
    }; 

}
