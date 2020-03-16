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
  static void printAttibutes(final Instances dataset) {
    final var top = dataset.numAttributes();
    for (int i = 0; i < top; ++i)
      System.out.println(dataset.attribute(i));
  }

  public static void main(String[] args) throws Exception {
    final var path = "all_tickets.arff";
    var dataset = new Instances(new BufferedReader(new FileReader(path)));

    for (final var index : new int[] {9, 7, 6, 5, 4, 3, 1})
      dataset.deleteAttributeAt(index - 1);

    // Cleaning / Data preparation
    final var numericToNominal = new NumericToNominal();
    numericToNominal.setOptions(Utils.splitOptions("-R last"));
    numericToNominal.setInputFormat(dataset);
    final var datasetV2 = Filter.useFilter(dataset, numericToNominal);

    final var nominalToString = new NominalToString();
    nominalToString.setOptions(Utils.splitOptions("-C first"));
    nominalToString.setInputFormat(datasetV2);
    final var datasetV3 = Filter.useFilter(datasetV2, nominalToString);

    printAttibutes(datasetV3);
    System.out.println(datasetV3.numInstances());

    final var stringToWordVector = new StringToWordVector();
    stringToWordVector.setOptions(Utils.splitOptions("-R 1 -W 100 -N 0 -M 1"));
    stringToWordVector.setInputFormat(datasetV3);
    final var datasetV4 = Filter.useFilter(datasetV3, stringToWordVector);

    dataset = datasetV4;
    System.out.println(dataset.numAttributes());

    // classify
    final var classifier = new J48();
    classifier.setOptions(Utils.splitOptions("-U -B -M 15 -batch-size 64"));
    dataset.setClassIndex(0);
    dataset.randomize(new java.util.Random(0));

    final var n = dataset.numInstances();

    final var percent = 0.80;
    final var trainSize = (int)Math.round(n * percent);
    final var testSize = n - trainSize;

    final var train = new Instances(dataset, 0, trainSize);
    final var test = new Instances(dataset, trainSize, testSize);

    classifier.buildClassifier(train);

    final var eval = new Evaluation(train);
    eval.evaluateModel(classifier, test);
    System.out.println(eval.toSummaryString());

    SerializationHelper.write("./j48.model", classifier);
  }
}
