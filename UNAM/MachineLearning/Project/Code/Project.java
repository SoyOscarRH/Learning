import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import weka.classifiers.Classifier;
import weka.classifiers.Evaluation;
import weka.classifiers.trees.J48;

import weka.core.Instances;
import weka.core.Utils;
import weka.core.tokenizers.WordTokenizer;
import weka.core.SerializationHelper;

import weka.filters.Filter;
import weka.filters.unsupervised.attribute.NominalToString;
import weka.filters.unsupervised.attribute.NumericToNominal;
import weka.filters.unsupervised.attribute.StringToWordVector;

class Project {
  /*Show all attribute names and type to the screen*/
  static void printAttibutes(final Instances dataset) {
    final var top = dataset.numAttributes();
    for (int i = 0; i < top; ++i)
      System.out.println(dataset.attribute(i));
  }

  public static void main(String[] args) throws Exception {
    final String DATASET_FILE = "all_tickets.arff";
    final double PERCENTAGE_SPLIT = 0.8;
    final int[] ROWS_TO_DELETE = new int[] {9, 7, 6, 5, 4, 3, 1};
    final String STWV_OPTIONS = "-R 1 -W 100 -N 0 -M 1";
    final String CLASSIFIER_OPTIONS = "-U -B -M 15 -batch-size 64";

    final String MODEL_SAVE = "j48.model";
    final String FILTER_SAVE = "stwv.filter";

    // Get the original dataset
    final var path = DATASET_FILE;
    var dataset = new Instances(new BufferedReader(new FileReader(path)));

    // Delete all the fields we will not use now
    for (final var index : ROWS_TO_DELETE)
      dataset.deleteAttributeAt(index - 1);

    // Cleaning / Data preparation
    // Transform urgency into nominal
    final var numericToNominal = new NumericToNominal();
    numericToNominal.setOptions(Utils.splitOptions("-R last"));
    numericToNominal.setInputFormat(dataset);
    final var datasetV2 = Filter.useFilter(dataset, numericToNominal);

    // Transform body into string
    final var nominalToString = new NominalToString();
    nominalToString.setOptions(Utils.splitOptions("-C first"));
    nominalToString.setInputFormat(datasetV2);
    final var datasetV3 = Filter.useFilter(datasetV2, nominalToString);

    printAttibutes(datasetV3);
    System.out.println(datasetV3.numInstances());

    // Transform body into a word vector
    final var stringToWordVector = new StringToWordVector();
    stringToWordVector.setOptions(Utils.splitOptions(STWV_OPTIONS));
    stringToWordVector.setInputFormat(datasetV3);
    final var datasetV4 = Filter.useFilter(datasetV3, stringToWordVector);

    dataset = datasetV4;
    System.out.println(dataset.numAttributes());

    // Create the classifier
    final var classifier = new J48();
    classifier.setOptions(Utils.splitOptions(CLASSIFIER_OPTIONS));
    dataset.setClassIndex(0);
    dataset.randomize(new java.util.Random(0));

    // Split and create a train and test dataset
    final var n = dataset.numInstances();

    final var percent = PERCENTAGE_SPLIT;
    final var trainSize = (int)Math.round(n * percent);
    final var testSize = n - trainSize;

    final var train = new Instances(dataset, 0, trainSize);
    final var test = new Instances(dataset, trainSize, testSize);

    // Build the classifier
    classifier.buildClassifier(train);

    // Evaluate the classifier using test dataset
    final var eval = new Evaluation(train);
    eval.evaluateModel(classifier, test);
    System.out.println(eval.toSummaryString());

    // Save them
    SerializationHelper.write(MODEL_SAVE, classifier);
    SerializationHelper.write(FILTER_SAVE, stringToWordVector);
  }
}
