// ignore_for_file: file_names

U? map<T, U>(U Function(T) mapping, T? opt) {
  return opt == null ? null : mapping(opt);
}