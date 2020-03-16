import java.io.BufferedReader;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Arrays;
import weka.classifiers.Classifier;
import weka.classifiers.trees.J48;
import weka.core.Instances;
import weka.core.SerializationHelper;
import weka.core.Utils;
import weka.filters.Filter;
import weka.filters.unsupervised.attribute.StringToWordVector;
class Tests {

  static void saveToFile(String filename, String text) throws Exception {
    try (final var out = new PrintStream(new FileOutputStream(filename))) {
      out.print(text);
    }
  }

  public static void main(String[] args) throws Exception {
    final var path = "test.arff";

    // Create necessary file using the data given
    saveToFile(path, String.format("@relation test\n"
                                       + "@attribute body string\n"
                                       + "@attribute urgency {0, 1, 2, 3}\n"
                                       + "@data\n"
                                       + "'%s',0",
                                   String.join(" ", args)));

    // Create the instance
    var tests = new Instances(new BufferedReader(new FileReader(path)));

    // Prepate the dataset
    final J48 classifier = (J48)SerializationHelper.read("./j48.model");
    final var stringToWordVector =
        (StringToWordVector)SerializationHelper.read("./stwv.filter");
    tests = Filter.useFilter(tests, stringToWordVector);
    tests.setClassIndex(0);

    // Classify
    for (int i = 0; i < tests.numInstances(); i++) {
      final var score = classifier.classifyInstance(tests.instance(i));
      final var prediction = tests.classAttribute().value((int)score);
      System.out.println(prediction);
    }
  }
}
