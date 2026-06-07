module Type where
import Control.Monad.Trans.Except
import Control.Monad.State
import System.Environment
import System.IO
import AST
import Tokenize
import Error
import Data.List

size_of (Type _ size) = size
array_of base len = Type (ARRAY base len) (size_of base * len)
func_type ret_type args = Type (FUNC ret_type args [] (-99)) 8
pointer_to base = Type (PTR base) 8
make_int  = Type INT 8
make_char = Type CHAR 1

is_integer :: Type -> Bool
is_integer t = case typeKind t of
  CHAR -> True
  INT -> True
  _   -> False

get_ptr_base :: Type -> Maybe Type
get_ptr_base t = case typeKind t of
  PTR   base -> Just base
  ARRAY base _ -> Just base
  _   -> Nothing


last' :: [a] -> a
last' ys = foldl1 (\_ -> \x -> x) ys


change_type new (Node kind _ tok) = Node kind new tok
