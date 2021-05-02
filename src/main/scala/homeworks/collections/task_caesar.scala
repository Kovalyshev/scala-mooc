package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_caesar {
  private val chars = ('A' to 'Z').toVector
  private val length = chars.size
  private val start = 'A'.toInt

  /**
   * В данном задании Вам предлагается реализовать функции,
   * реализующие кодирование/декодирование строки шифром Цезаря.
   * https://ru.wikipedia.org/wiki/Шифр_Цезаря
   * Алфавит - прописные латинские буквы от A до Z.
   * Сдвиг   - неотрицательное целое число.
   * Пример: при сдвиге 2 слово "SCALA" шифруется как "UECNC".
   */
  /**
   * @param word   входное слово, которое необходимо зашифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return зашифрованное слово
   */
  def encrypt(word: String, offset: Int): String = {
    val localOffset = offset % length
    word.map(ch => {
      val code = (ch.toInt + localOffset - start) % length
      chars(code)
    })
  }

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String = {
    val localOffset = offset % length
    cipher.map(ch => {
      val code = (ch.toInt - localOffset + length - start) % length
      chars(code)
    })
  }

}
