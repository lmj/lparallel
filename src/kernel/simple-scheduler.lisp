;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:lparallel.kernel)

(alias-function biased-queue-lock lparallel.biased-queue::lock)

(alias-function make-scheduler make-biased-queue)

(defun/type schedule-task (scheduler task priority) (scheduler task t) t
  (declare #.*normal-optimize*)
  (ccase priority
    (:default (push-biased-queue     task scheduler))
    (:low     (push-biased-queue/low task scheduler))))

(defun/inline next-task (scheduler worker)
  (declare (ignore worker))
  (pop-biased-queue scheduler))

(defun/type steal-task (scheduler) (scheduler) (or task null)
  (with-lock-predicate/wait
      (biased-queue-lock scheduler)
      (not (biased-queue-empty-p/no-lock scheduler))
    ;; don't steal nil, the end condition flag
    (when (peek-biased-queue/no-lock scheduler)
      (pop-biased-queue/no-lock scheduler))))

(setf (macro-function 'with-locked-scheduler)
      (macro-function 'with-locked-biased-queue))

(alias-function scheduler-empty-p/no-lock biased-queue-empty-p/no-lock)

(defun/type distribute-tasks/no-lock (scheduler tasks) (scheduler sequence) t
  (dosequence (task tasks)
    (push-biased-queue/no-lock task scheduler)))
