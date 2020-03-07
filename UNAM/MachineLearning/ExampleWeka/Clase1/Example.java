import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import weka.classifiers.Classifier;
import weka.classifiers.Evaluation;
import weka.classifiers.bayes.NaiveBayes;
import weka.classifiers.functions.SMO;
import weka.classifiers.trees.J48;
import weka.core.Instances;
import weka.core.converters.ConverterUtils.DataSource;


class TreeJ48 {
  int option = 0;

  Classifier createModel(final Instances train) throws Exception {
    final var classifier = new J48();
    classifier.buildClassifier(train);

    return classifier;
  }

  ArrayList<Instances> selectData(String path) throws Exception {
    final var data = new ArrayList<Instances>();
    final var train = new Instances(new BufferedReader(new FileReader(path)));

    System.out.println(
        "Elige uno de los siguientes atributos para clasificar:");

    for (var i = 0; i < train.numAttributes(); i++) {
      System.out.println(Integer.toString(i) + ") " + train.attribute(i));
    }

    final var option = (char)System.in.read();
    final var valOption = Character.getNumericValue(option);
    System.out.println(train.attribute(valOption));
    train.setClassIndex(valOption);
    data.add(train);

    Instances test = new Instances(new BufferedReader(new FileReader(path)));
    test.setClassIndex(valOption);
    data.add(test);

    return data;
  }
}

public class Example {
  public static void main(String[] args) throws IOException, Exception {
    final var tree = new TreeJ48();

    final var path = "videogames.arff";
    final var path_test = path;

    final var data = tree.selectData(path);
    final var train = data.get(0);
    final var test = data.get(1);

    final var classifier = tree.createModel(train);
    final var evaluation = new Evaluation(train);

    evaluation.evaluateModel(classifier, test);

    System.out.println(evaluation.toSummaryString("", false));
    final var unlabeled = test;

    unlabeled.setClassIndex(tree.option);
    final var labeled = new Instances(unlabeled);

    for (int i = 0; i < unlabeled.numInstances(); i++) {
      final var label = classifier.classifyInstance(unlabeled.instance(i));
      labeled.instance(i).setClassValue(label);
      System.out.println(unlabeled.instance(i) + "=>" + Double.toString(label));
    }
  }
}
