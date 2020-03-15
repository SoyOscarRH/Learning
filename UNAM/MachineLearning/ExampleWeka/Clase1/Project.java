import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import weka.classifiers.Classifier;
import weka.classifiers.Evaluation;
import weka.classifiers.trees.J48;
import weka.core.*;
import weka.core.Instances;
import weka.core.stemmers.SnowballStemmer;
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
    numericToNominal.setOptions(new String[] {"-R", "last"});
    numericToNominal.setInputFormat(dataset);
    final var datasetV2 = Filter.useFilter(dataset, numericToNominal);

    final var nominalToString = new NominalToString();
    nominalToString.setOptions(new String[] {"-C", "1"});
    nominalToString.setInputFormat(datasetV2);
    final var datasetV3 = Filter.useFilter(datasetV2, nominalToString);

    printAttibutes(datasetV3);

    final var stringToWordVector = new StringToWordVector();
    stringToWordVector.setOptions(new String[] {
        "-R", "1", "-W", "100", "-prune-rate", "-1.0", "-N", "0",
        "-stopwords-handler", "weka.core.stopwords.Null", "-M", "2",
        "-tokenizer",
        "weka.core.tokenizers.WordTokenizer -delimiters \" \\r\\n\\t.,;:\\\'\\\"()?!\""});

    stringToWordVector.setInputFormat(datasetV3);
    final var datasetV4 = Filter.useFilter(datasetV3, stringToWordVector);

    dataset = datasetV4;
    System.out.println(dataset.numAttributes());


    // classify
    final var classifier = new J48();
    classifier.setOptions(new String[] {"-U", "-M", "5", "-batch-size", "64"});
    dataset.setClassIndex(0);

    final var percent = 0.80;
    final var trainSize = (int)Math.round(dataset.numInstances() * percent / 100);
    final var testSize = dataset.numInstances() - trainSize;

    final var train = new Instances(dataset, 0, trainSize);
    final var test = new Instances(dataset, trainSize, testSize);

    classifier.buildClassifier(train);

    final var eval = new Evaluation(train);
    eval.evaluateModel(classifier, test);
    System.out.println(eval.toSummaryString("", false));
  }
}
