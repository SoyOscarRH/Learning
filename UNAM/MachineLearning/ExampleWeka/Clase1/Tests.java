import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import weka.classifiers.Classifier;
import weka.classifiers.trees.J48;
import weka.core.Instances;
import weka.core.SerializationHelper;
import weka.core.Utils;
import weka.filters.Filter;
import weka.filters.unsupervised.attribute.StringToWordVector;

class Tests {
  public static void main(String[] args) throws Exception {
    final var path = "test.arff";
    var tests = new Instances(new BufferedReader(new FileReader(path)));

    final var stringToWordVector = new StringToWordVector();
    stringToWordVector.setOptions(Utils.splitOptions("-R 1 -W 100 -N 0 -M 1"));
    stringToWordVector.setInputFormat(tests);
    tests = Filter.useFilter(tests, stringToWordVector);
    tests.setClassIndex(0);

    final J48 classifier = (J48)SerializationHelper.read("./j48.model");

    for (int i = 0; i < tests.numInstances(); i++) {
      final var score = classifier.classifyInstance(tests.instance(i));
      final var prediction = tests.classAttribute().value((int)score);
      System.out.println(prediction);
    }
  }
}
