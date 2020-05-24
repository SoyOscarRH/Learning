class Emotion {
  static final String smile = "grinning-face_1f600.png";
  static final String tongue = "winking-face-with-tongue_1f61c.png";
  static final String pacman = "squinting-face-with-tongue_1f61d.png";
  static final String happy = "slightly-smiling-face_1f642.png";
  static final String sad = "frowning-face_2639.png";
  static final String kiss = "kiss-mark_1f48b.png";

  static final String url = "https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/";
  static final String type = "thumbs/240/google/241/";

  private static String createHTMLImage(final String path) {
    return String.format("<img width=12 src=%s%s%s />", url, type, path);
  }

  public static String replaceEmotions(final String message) {
    final var result = message.replaceAll(":\\)", Emotion.createHTMLImage(Emotion.smile))
                           .replaceAll(":P", Emotion.createHTMLImage(Emotion.tongue))
                           .replaceAll(":v", Emotion.createHTMLImage(Emotion.pacman))
                           .replaceAll(":D", Emotion.createHTMLImage(Emotion.happy))
                           .replaceAll(":\\(", Emotion.createHTMLImage(Emotion.sad))
                           .replaceAll(":\\*", Emotion.createHTMLImage(Emotion.kiss));

    return result;
  }
}